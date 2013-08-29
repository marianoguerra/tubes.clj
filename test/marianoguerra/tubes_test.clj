(ns marianoguerra.tubes-test
  (:require
    [ring.mock.request :as req]
    [cemerick.friend :as friend]
    [marianoguerra.rate-meter :as rm])
  (:use marianoguerra.tubes
        marianoguerra.pipe
        clojure.test
        [cheshire.core :only (parse-string generate-string)]))

(def text-content-type "text/plain")

(defn request-with-body [path body method & [content-type]]
  (req/content-type
    (req/request method path body)
    (or content-type "application/json")))

(def entity {:username "bob" :password "secret" :age 27 :admin false})
(def entity-json (generate-string entity))

(defn object-schema [required properties]
  {:type "object"
   :required required
   :properties properties
   :additionalProperties false})

(defn type-string [min-length] {:type "string" :minLength min-length})
(def type-boolean {:type "boolean"})
(defn type-integer-min-max [minimum maximum]
  {:type "integer" :minimum minimum :maximum maximum})

(def entity-schema (object-schema [:username :password :age :admin]
                                  {:username (type-string 3)
                                   :password (type-string 3)
                                   :age (type-integer-min-max 0 150)
                                   :admin type-boolean}))

;; example:
;; {:current admin,
;;  :authentications {admin {:identity admin, :_id "1", :_rev "1-9",
;;                           :username admin, :email "", :roles (:admin),
;;                           :data {:nextPasswordUpdate 0}, :t "user"}}}
(defn- make-identity [& {:keys [identity username roles]}]
  (let [identity (or identity "bob")
        username (or username identity "bob")
        roles (or roles [:user])]
    {:current username
     :authentications
     {username
      {:identity identity
       :username username
       :roles roles}}}))

(defn- attach-identity [request & [identity]]
  (assoc request :session {::friend/identity (or identity (make-identity))}))

(defn post [path body & [content-type]]
  (request-with-body path body :post (or content-type json-content-type)))

(defn put [path body & [content-type]]
  (request-with-body path body :put (or content-type json-content-type)))

(defn error-type-is [result type]
  (is (= (get-in result [:type]) type)))

(defn error-reason-is [result reason]
  (is (= (get-in result [:reason]) reason)))

(def is-not-error? (complement error?))

(deftest tubes-test
  (testing "is-json-content-type"
           (let [req-json (post "/session" entity-json)
                 req-text (post "/session" entity-json text-content-type)

                 json-result (pipe req-json is-json-content-type)
                 text-result (pipe req-text is-json-content-type)]

             (is (not (error? json-result)))
             (is (error? text-result))
             (error-type-is text-result :bad-request)))

  (testing "conforms-to-json-schema"
           (let [req (post "/session" entity-json)
                 result-ok (pipe req
                                 is-json-content-type
                                 extract-json-body
                                 (conforms-to-json-schema entity-schema))

                 req-over-age (post "/session" (generate-string
                                                 {:username "asd"
                                                  :password "secret"
                                                  :age 200
                                                  :admin false}))
                 result-error (pipe req-over-age
                                 is-json-content-type
                                 extract-json-body
                                 (conforms-to-json-schema entity-schema))]

             (is (is-not-error? result-ok))
             (is (error? result-error))
             (error-type-is result-error :bad-request)
             (error-reason-is result-error "object doesn't validate against schema")))

  (testing "dispatch-by-path"
           (let [req1 (post "/session" entity-json)
                 req2 (post "/user" entity-json)
                 req3 (post "/person" entity-json)
                 session-val {:value "session"}
                 user-val {:value "user"}

                 dispatcher (dispatch-by-path {"/session" (fn [_] (continue session-val))
                                                 "/user" (fn [_] (continue user-val))})

                 result-session (pipe req1 dispatcher)
                 result-user (pipe req2 dispatcher)
                 result-not-found (pipe req3 dispatcher)]

             (is (is-not-error? result-session))
             (is (is-not-error? result-user))
             
             (is (= result-session session-val))
             (is (= result-user user-val))

             (is (= (:reason result-not-found) "no handler for: /person"))
             (is (= (:type result-not-found) :not-found))))

  (testing "dispatch-by-method"
           (let [req1 (post "/session" entity-json)
                 req2 (put "/user" entity-json)
                 req3 (assoc req1 :request-method :patch)
                 post-val {:value "post"}
                 put-val {:value "put"}

                 dispatcher (dispatch-by-method {:post (fn [_] (continue post-val))
                                               :put  (fn [_] (continue put-val))})

                 result-post (pipe req1 dispatcher)
                 result-put (pipe req2 dispatcher)
                 result-not-found (pipe req3 dispatcher)]

             (is (is-not-error? result-post))
             (is (is-not-error? result-put))
             (is (error? result-not-found))
             
             (is (= result-post post-val))
             (is (= result-put put-val))

             (error-type-is result-not-found :method-not-supported)
             (error-reason-is result-not-found "no handler for method: patch")))


  (testing "has-one-of-roles"
           (let [req (attach-identity (post "/session" entity-json))

                 result-ok (pipe req (has-one-of-roles :user :admin))
                 result-error (pipe req (has-one-of-roles :admin :root))]

             (is (is-not-error? result-ok))
             (is (error? result-error))
             (error-type-is result-error :unauthorized)
             (error-reason-is result-error "expected one of roles: admin, root")))

  (testing "is-authenticated"
           (let [req (post "/session" entity-json)
                 req-auth (attach-identity req)

                 result-ok (pipe req-auth is-authenticated)
                 result-error (pipe req is-authenticated)]

             (is (is-not-error? result-ok))
             (is (error? result-error))
             (error-type-is result-error :unauthorized)
             (error-reason-is result-error "not authenticated")))

  (testing "rate-limit"
           (let [req (post "/session" entity-json)
                 req-auth (attach-identity req)
                 markers {:one-min rm/minute-mark :five-mins (rm/minute-step-mark 5)}
                 tags (keys markers)
                 rate-store (rm/memory-rate-store markers)
                 limit-provider (constant-limit-provider 1)
                 rate-limiter (rate-limit rate-store tags username-from-request
                                          limit-provider)
                 handler (fn [req] {:ok true})
                 result-1 (pipe req-auth rate-limiter handler)
                 result-2 (pipe req-auth rate-limiter handler)
                 result-response (pipe result-2 to-ring-response)]

             (is (= result-1 {:ok true}))
             (is (= result-2 {:reason "rate limit reached"
                              :id "bob"
                              :type :too-many-requests}))
             (is (error? result-2))
             (is (= (:status result-response) 429))))

  (testing "response mappers work"
           (let [req (attach-identity (post "/session" entity-json))
                 in-pipe (compose (has-one-of-roles :user :admin)
                                  extract-json-body)
                 out-pipe (compose to-ring-response response-body-to-json)

                 result-ok (pipe req in-pipe out-pipe)
                 in-pipe-1 (compose (has-one-of-roles :admin :root)
                                    extract-json-body)
                 result-error (-> req (pipe in-pipe-1) (pipe out-pipe))]

             (is (is-not-error? result-ok))
             (is (error? result-error))
             (is (= (:status result-error 401)))
             (is (= (-> result-error :body (parse-string true) :reason)
                    "expected one of roles: admin, root"))))

  (testing "mark-as-ring-response marks the response"
    (is (ring-response? (mark-as-ring-response {})))
    (is (not (ring-response? {}))))

  (testing "to-ring-response returns value as is if marked as response"
    (let [response (mark-as-ring-response {:value 42})]
      (is (= response (to-ring-response response)))
      (is (not= response (to-ring-response {:value 42})))))

  (testing "extract-json-body"
           (let [req (post "/session" entity-json)
                 result (pipe req is-json-content-type extract-json-body)]

             (is (not (error? result)))
             (is (= result entity))
             (is (= (:request (meta result)) req)))))
