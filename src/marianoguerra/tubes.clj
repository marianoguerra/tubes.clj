(ns marianoguerra.tubes
  (:require
    [closchema.core :as schema]
    [cemerick.friend :as friend])
  (:use marianoguerra.pipe
        [cheshire.core :only (parse-string generate-string)]))

(def json-content-type "application/json")
(def text-content-type "text/plain")

;; utilities

(defn make-tube [& funs]
  "build a function that when called with an object will pipe it throug
  funs, it can be used to compose more than one op into a higher level one"
  (fn [obj]
    (apply (partial pipe obj) funs)))

(defn http-response [data & [status headers]]
  {:status (or status 200)
   :headers (or headers {})
   :body data})

(defn json-response [data & [status headers]]
  (http-response (generate-string data)
                 status
                 (assoc headers "Content-Type" json-content-type)))

(defn text-response [data & [status headers]]
  (http-response data
                 status
                 (assoc headers "Content-Type" text-content-type)))

(defn throw-on-unknown-status [status request]
  (throw (IllegalArgumentException.
           (str "unknown status: " (name status) request))))

(defn type-to-status [status extra-status-handler & [response]]
  (case status
    :ok 200
    :created 201
    :accepted 202
    :no-content 204
    :moved 301
    :found 302
    :see-other 303
    :not-modified 304
    :bad-request 400
    :unauthorized 401
    :forbidden 403
    :not-found 404
    :method-not-supported 405
    :conflict 409
    :length-required 411
    :entity-too-large 413
    :too-many-requests 429
    :error 500
    :not-implemented 501
    :bad-gateway 502
    :service-unavailable 503
    :gateway-timeout 504
    :timeout 504

    (extra-status-handler status response)))

(defn at-least-one-ok [& funs]
  (fn [obj]
    (apply (partial first-ok-pipe obj) funs)))

;; request handlers

(defn is-json-content-type [req]
    (let [content-type (or (:content-type req) "")]
      (if (or (= content-type "application/json")
              (= (.indexOf content-type "application/json;") 0))
        (ok req)
        (error {:reason "expected content type to be: application/json"
                :actual content-type
                :type :bad-request}))))

(defn method-is [method]
  (fn [req]
    (if (= (:request-method req) method)
      (ok req)
      (error {:reason (str "expected method to be: " (name method))
              :actual (:request-method req)
              :type  :method-not-supported}))))

(defn method-is-one-of [& methods]
  (let [methods-set (set methods)]
    (fn [req]
      (if (contains? methods-set (:request-method req))
        (ok req)
        (error {:reason (str "expected method to be one of: "
                             (clojure.string/join ", " (map name methods)))
                :actual (:request-method req)
                :type :method-not-supported})))))

(defn- normalize-path [path]
  (if (= (last path) \/)
    path
    (str path "/")))

(defn dispatch-by-path [path-to-tube]
  "receive a map with path as key and handler as value, dispatch to handler if
  (or :path-info :uri) is the same as key in map, try with and without ending
  slash for convenience"
  ; add / to the end of all keys in the api-map
  (let [routes (apply hash-map
                      (mapcat
                        (fn [[key val]] [(normalize-path key) val])
                        (seq path-to-tube)))]
    (fn [req]
      (let [path (or (:path-info req) (:uri req))
            tube (get routes (normalize-path path))]

        (if tube
          (tube req)
          (text-response (str "no handler for: " path) 404))))))

(defn dispatch-by-method [method-to-tube]
  "receive a map with method as key and handler as value, dispatch to handler
  if :request-method is the same as key in map"
  (fn [req]
    (let [method (:request-method req)
          tube (get method-to-tube method)]
      (if tube
        (tube req)
        (error {:reason (str "no handler for method: " (name method))
                :actual method
                :type :method-not-supported})))))

(defn has-one-of-roles [& roles]
  (fn [req]
    (if (friend/authorized? roles (friend/identity req))
      (ok req)
      (error {:reason (str "expected one of roles: "
                           (clojure.string/join ", " (map name roles)))
              :actual (-> (friend/identity req)
                          friend/current-authentication
                          :roles)
              :type :unauthorized}))))

(defn is-authenticated [req]
  (if (friend/current-authentication req)
    (ok req)
    (error {:reason "not authenticated"
            :type :unauthorized})))

;; request -> object handlers

(defn extract-json-body [req]
  (let [json-body (parse-string (slurp (:body req)) true)]

    (ok (with-meta json-body {:request req}))))

;; object handlers

(defn conforms-to-json-schema [schema]
  (fn [obj]
    (if (schema/validate schema obj)
      (ok obj)
      (error {:reason "object doesn't validate against schema"
              :schema schema
              :type   :bad-request}))))

;; response handlers

(defn http-response-body-to-json [response]
  (let [headers (:headers response)
        new-headers (assoc headers "Content-Type" json-content-type)
        body (:body response)
        new-body (generate-string body)]

    (assoc response :headers new-headers :body new-body)))

(defn response-to-http-response [extra-status-handler]
  (fn [response]
    (let [{type :type data :data} response]
      (if (= type :ok)
        (http-response data)
        (http-response data (type-to-status (get-in response [:data :type])
                                            extra-status-handler response))))))

(defn change-http-status [status-map]
  "receive a map of values to match as keys and replacements as values,
  change the status of response to value if current status is key"
  (fn [response]
    (let [status (:status response)
          new-status (or (get status-map status) status)
          new-response (assoc response :status new-status)]
      new-response)))
