(ns marianoguerra.tubes-test
  (:require
    [ring.mock.request :as req]
    [cemerick.friend :as friend])
  (:use marianoguerra.tubes
        marianoguerra.pipe
        clojure.test
        [cheshire.core :only (parse-string generate-string)]))

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
  (is (= (get-in result [:data :type]) type)))

(defn error-reason-is [result reason]
  (is (= (get-in result [:data :reason]) reason)))

(deftest tubes-test
  (testing "is-json-content-type"
           (let [req-json (post "/session" entity-json)
                 req-text (post "/session" entity-json text-content-type)

                 json-result (pipe req-json is-json-content-type)
                 text-result (pipe req-text is-json-content-type)]

             (is (ok? json-result))
             (is (not (ok? text-result)))
             (error-type-is text-result :bad-request)))

  (testing "method-is"
           (let [req (post "/session" entity-json)
                 result-is-post (pipe req (method-is :post))
                 result-is-put (pipe req (method-is :put))]

             (is (ok? result-is-post))
             (error-reason-is result-is-put "expected method to be: put")
             (error-type-is result-is-put :method-not-supported)))

  (testing "method-is-one-of"
           (let [req (post "/session" entity-json)
                 result-is-ok (pipe req (method-is-one-of :get :post))
                 result-is-error (pipe req (method-is-one-of :get :delete :put))]

             (is (ok? result-is-ok))
             (error-reason-is result-is-error
                              "expected method to be one of: get, delete, put")
             (error-type-is result-is-error :method-not-supported)))

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

             (is (ok? result-ok))
             (is (not (ok? result-error)))
             (error-type-is result-error :bad-request)
             (error-reason-is result-error "object doesn't validate against schema")))

  (testing "make-tube"
           (let [req (post "/session" entity-json)
                 extract-json-body-from-post-request (make-tube
                                                       (method-is :post)
                                                       is-json-content-type
                                                       extract-json-body)
                 result-ok (pipe req
                                 extract-json-body-from-post-request
                                 (conforms-to-json-schema entity-schema))]

             (is (ok? result-ok))))

  (testing "dispatch-by-path"
           (let [req1 (post "/session" entity-json)
                 req2 (post "/user" entity-json)
                 req3 (post "/person" entity-json)

                 dispatcher (dispatch-by-path {"/session" (fn [_] (ok "session"))
                                                 "/user" (fn [_] (ok "user"))})

                 result-session (pipe req1 dispatcher)
                 result-user (pipe req2 dispatcher)
                 result-not-found (pipe req3 dispatcher)]

             (is (ok? result-session))
             (is (ok? result-user))
             
             (is (= (:data result-session) "session"))
             (is (= (:data result-user) "user"))

             (is (= (:body result-not-found) "no handler for: /person"))
             (is (= (:status result-not-found) 404))
             (is (= (:headers result-not-found) {"Content-Type" "text/plain"}))))

  (testing "dispatch-by-method"
           (let [req1 (post "/session" entity-json)
                 req2 (put "/user" entity-json)
                 req3 (assoc req1 :request-method :patch)

                 dispatcher (dispatch-by-method {:post (fn [_] (ok "post"))
                                               :put  (fn [_] (ok "put"))})

                 result-post (pipe req1 dispatcher)
                 result-put (pipe req2 dispatcher)
                 result-not-found (pipe req3 dispatcher)]

             (is (ok? result-post))
             (is (ok? result-put))
             (is (not (ok? result-not-found)))
             
             (is (= (:data result-post) "post"))
             (is (= (:data result-put) "put"))

             (error-type-is result-not-found :method-not-supported)
             (error-reason-is result-not-found "no handler for method: patch")))

  (testing "response-to-http-response"
           (let [ok-resp ((response-to-http-response throw-on-unknown-status) (ok 42))
                 not-found-resp ((response-to-http-response throw-on-unknown-status)
                                                           (error {:type :not-found}))
                 custom-status ((response-to-http-response (fn [status _] [status 1000]))
                                (error {:type :hi}))]

             (is (= (:status ok-resp) 200))
             (is (= (:status not-found-resp) 404))

             (is (= (:status custom-status) [:hi 1000]))
             (is  (thrown? IllegalArgumentException
                           ((response-to-http-response throw-on-unknown-status)
                                                      (error {:type :asd}))))))

  (testing "response-to-http-response with :response-headers metadata"
           (let [ok-resp ((response-to-http-response throw-on-unknown-status)
                       (with-meta (ok 42)
                                  {:response-headers {"X-Custom" "hi"}}))
                 err-resp ((response-to-http-response throw-on-unknown-status)
                       (with-meta (error {:type :error})
                                  {:response-headers {"X-Custom" "hi"}}))]

             (println ok-resp err-resp)
             (is (= (get-in ok-resp  [:headers "X-Custom"]) "hi"))
             (is (= (get-in err-resp [:headers "X-Custom"]) "hi"))))

  (testing "http-response-body-to-json"
           (let [http-response (assoc (http-response {:a 1 :b false} 201
                                                     {"X-Custom" "hi"})
                                      :custom 42)
                 json-response (http-response-body-to-json http-response)]

             (is (= (:custom json-response) 42) "retains custom fields")
             (is (= (:status json-response) 201) "retains status code")
             (is (= (get-in json-response [:headers "X-Custom"]) "hi")
                 "retains old headers")
             (is (= (get-in json-response [:headers "Content-Type"])
                    json-content-type))

             (is (= (parse-string (:body json-response) true) {:a 1 :b false}))))

  (testing "has-one-of-roles"
           (let [req (attach-identity (post "/session" entity-json))

                 result-ok (pipe req (has-one-of-roles :user :admin))
                 result-error (pipe req (has-one-of-roles :admin :root))]

             (is (ok? result-ok))
             (is (not (ok? result-error)))
             (error-type-is result-error :unauthorized)
             (error-reason-is result-error "expected one of roles: admin, root")))

  (testing "is-authenticated"
           (let [req (post "/session" entity-json)
                 req-auth (attach-identity req)

                 result-ok (pipe req-auth is-authenticated)
                 result-error (pipe req is-authenticated)]

             (is (ok? result-ok))
             (is (not (ok? result-error)))
             (error-type-is result-error :unauthorized)
             (error-reason-is result-error "not authenticated")))

  (testing "at-least-one-ok"
         (let [req (attach-identity (post "/session" entity-json))

               result-ok-1 (pipe req (at-least-one-ok
                                       (has-one-of-roles :admin :root)
                                       (has-one-of-roles :user :admin)))

               result-ok-2 (pipe req (at-least-one-ok
                                       (has-one-of-roles :user :admin)
                                       (has-one-of-roles :admin :root)))

               result-error (pipe req (at-least-one-ok
                                        (has-one-of-roles :admin :root)
                                        (has-one-of-roles :user-admin)))]

           (is (ok? result-ok-1))
           (is (ok? result-ok-2))
           (is (not (ok? result-error)))))

  (testing "change-status"
           (is (= ((change-http-status {200 201}) {:status 200})
                  {:status 201})))

  (testing "extract-json-body"
           (let [req (post "/session" entity-json)
                 {type :type result :data} (pipe req
                                                 is-json-content-type
                                                 extract-json-body)]

             (is (= type :ok))
             (is (= result entity))
             (is (= (:request (meta result)) req)))))
