(ns marianoguerra.tubes
  (:require
    [closchema.core :as schema]
    [cemerick.friend :as friend])
  (:use marianoguerra.pipe
        [cheshire.core :only (parse-string generate-string)]))

(def json-content-type "application/json")

;; utilities

(defn http-response [data & [status headers]]
  {:status (or status 200)
   :headers (or headers {})
   :body data})

(defn json-response [data & [status headers]]
  (http-response (generate-string data) status
                 (assoc headers "Content-Type" json-content-type)))

(def type-to-status {:ok 200
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
                     :timeout 504})

;; request handlers

(defn is-json-content-type [req]
    (let [content-type (or (:content-type req) "")]
      (if (or (= content-type "application/json")
              (= (.indexOf content-type "application/json;") 0))
        (continue req)
        (error {:reason "expected content type to be: application/json"
                :actual content-type
                :type :bad-request}))))

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
          (error {:reason (str "no handler for: " path)
                  :actual path
                  :type :not-found}))))))

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
      (continue req)
      (error {:reason (str "expected one of roles: "
                           (clojure.string/join ", " (map name roles)))
              :actual (-> (friend/identity req)
                          friend/current-authentication
                          :roles)
              :type :unauthorized}))))

(defn is-authenticated [req]
  (if (friend/current-authentication req)
    (continue req)
    (error {:reason "not authenticated"
            :type :unauthorized})))

;; request -> object handlers

(defn extract-json-body [req]
  (let [json-body (parse-string (slurp (:body req)) true)]

    (continue json-body {:request req})))

;; object handlers

(defn conforms-to-json-schema [schema]
  (fn [obj]
    (if (schema/validate schema obj)
      (continue obj)
      (error {:reason "object doesn't validate against schema"
              :schema schema
              :type   :bad-request}))))

;; response handlers

(defn to-ring-response [value]
  (let [meta-data (meta value)
        response-status-0 (or (:response-status meta-data) 200)
        response-status (if (error? value)
                          (or (get type-to-status (:type value))
                              response-status-0)
                          response-status-0)
        response-headers (or (:response-headers meta-data) {})]
    (continue (http-response value response-status response-headers))))

(defn response-body-to-json [response]
  (let [headers (:headers response)
        new-headers (assoc headers "Content-Type" json-content-type)
        body (:body response)
        new-body (generate-string body)]
    (continue (assoc response :headers new-headers :body new-body))))
