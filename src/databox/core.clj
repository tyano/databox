(ns databox.core
  (:refer-clojure :exclude [map mapcat filter distinct])
  (:require [clojure.core :as core]))

(declare map* mapcat* box failure unbox box? success? failure? success-value exception)

(defn- handle-boxed-data
  [boxed-data]
  (case (:type boxed-data)
    :success
    (:result boxed-data)

    :failure
    (throw (:exception boxed-data))))

(defrecord Box
  [type]

  clojure.lang.IDeref
  (deref [box] (handle-boxed-data box))

  clojure.lang.IBlockingDeref
  (deref [box ms timeout-value] (handle-boxed-data box)))

(defmethod print-method Box [v ^java.io.Writer w]
  (if (success? v)
    (.write w (str "Success[" (pr-str (success-value v)) "]"))
    (.write w (str "Failure[" (pr-str (exception v)) "]"))))

(defn map
  [data & args]
  (cond
    ;; transducer
    (fn? data)
    (let [f data
          [options & _] args]
      (fn [rf]
        (fn
          ([] (rf))
          ([result] (rf result))
          ([result v]
           (let [new-value (map* (box v) f options)]
             (rf result new-value))))))

    :else
    (let [obj data
          [f & [options]] args]
      (map* (box obj) f options))))


(defn mapcat
  [data & args]
  (cond
    ;; transducer
    (fn? data)
    (let [f data
          [options & _] args]
      (fn [rf]
        (fn
          ([] (rf))
          ([result] (rf result))
          ([result v]
           (let [new-values (mapcat* (box v) f options)]
             (reduce
              #(let [ret (rf %1 %2)]
                 (if (reduced? ret)
                   (reduced ret)
                   ret))
              result
              new-values))))))

    :else
    (let [obj data
          [f & [options]] args]
      (mapcat* (box obj) f options))))


(defn filter
  "filter transducer for filterling boxed data by the contained value."
  [pred]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (cond
         (failure? input)
         (rf result input)

         (pred (success-value (box input)))
         (rf result (box input))

         :else
         result)))))

(defn distinct
  []
  (fn [rf]
    (let [seen (volatile! #{})]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (cond
           (failure? input)
           (rf result input)

           :else
           (let [boxed (box input)
                 v (success-value boxed)]
             (if (contains? @seen v)
               result
               (do (vswap! seen conj v)
                   (rf result boxed))))))))))

(defn distinct-by
  [f]
  (fn [rf]
    (let [seen (volatile! #{})]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (cond
           (failure? input)
           (rf result input)

           :else
           (let [boxed (box input)
                 v (f (success-value boxed))]
             (if (contains? @seen v)
               result
               (do (vswap! seen conj v)
                   (rf result boxed))))))))))

(defn success
  [value]
  (map->Box {:type :success
             :result value}))

(defn failure
  [ex]
  (map->Box {:type :failure
             :exception ex}))

(defn success-value
  [v]
  (:result (box v)))

(defn exception
  [v]
  (:exception (box v)))

(defn box?
  [boxed]
  (instance? Box boxed))

(defn box
  [v]
  (cond
    (box? v)
    v

    (instance? Throwable v)
    (failure v)

    :else
    (success v)))

(def ^:deprecated value box)

(defn unbox
  [v]
  (if (box? v)
    @v
    v))

(defn success?
  [boxed]
  (= (:type boxed) :success))

(defn failure?
  [boxed]
  (= (:type boxed) :failure))


(defn strip-default-keys
  [boxed]
  (dissoc boxed :type :result :exception))

(defn- map*
  [boxed f & [{:keys [throw?] :or {throw? false}}]]
  (if (failure? boxed)
    (if throw?
      @boxed
      boxed)

    (try
      (let [unboxed (unbox boxed)
            r (box (f unboxed))]
        ;; we must keep keys assigned on a original boxed value.
        ;; so remove all default keys of Boxed instance from a original
        ;; boxed value and merge a new boxed instance on it.
        (-> (strip-default-keys boxed)
            (merge r)
            (map->Box)))

      (catch Throwable th
        (if throw?
          (throw th)
          (-> (strip-default-keys boxed)
              (merge (failure th))
              (map->Box)))))))

(defn- mapcat*
  [boxed f & [{:keys [throw?] :or {throw? false}}]]
  (if (failure? boxed)
    (if throw?
      @boxed
      boxed)

    (try
      (let [unboxed (unbox boxed)
            r (box (f unboxed))]
        ;; r is a boxed object containing a coll.
        ;; we must convert it to a coll of boxed objects.
        ;; we can use map* for process the boxed value.
        (let [stripped (strip-default-keys boxed)]
          (if (failure? r)
            (-> (merge stripped r)
                (map->Box))
            (unbox (map* r (fn [coll] (core/map #(-> stripped
                                                     (merge r)
                                                     (assoc :result %)
                                                     (map->Box))
                                                coll)))))))
      (catch Throwable th
        (if throw?
          (throw th)
          (-> (strip-default-keys boxed)
              (merge (failure th))
              (map->Box)))))))

(defmacro maplet
  [& args]
  (let [has-opts? (not (vector? (first args)))
        [opts binding] [(if has-opts? (first args) {})
                        (if has-opts? (second args) (first args))]
        body (if has-opts? (drop 2 args) (drop 1 args))
        v (first binding)
        boxed (second binding)]
    `(map (box ~boxed)
          (fn [~v]
            (do ~@body))
          ~opts)))
