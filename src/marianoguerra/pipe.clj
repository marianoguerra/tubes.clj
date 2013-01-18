(ns marianoguerra.pipe)

(defrecord Result [type data])

(defn result [type & [data]]
  (->Result type data))

(defn ok [& [data]]
  (result :ok data))

(defn error [& [data]]
  (result :error data))

(defn ok? [result]
  (= (:type result) :ok))

(defn pipe [obj & funs]
  (if (seq funs)
    (let [result ((first funs) obj)
          {type :type data :data} result]
      (if (= type :ok)
        (recur data (rest funs))
        result))

    (ok obj)))

(defn first-ok-pipe [obj & funs]
  "will return the first passing check, if all fail will return the last one,
  you should avoid doing mutation on the passed functions since you don't know
  which ones will execute and when a test fails the next one will be called with
  the last successfully returned object"
  (if (seq funs)
    (let [result ((first funs) obj)
          {type :type data :data} result]
      (if (= type :ok)
        result
        (if (seq (rest funs))
          ; it not ok but remaining checks
          (recur obj (rest funs))
          ; if no remaining checks and error return this
          result)))

      (error {:reason "no checks to apply"
              :type :error})))
