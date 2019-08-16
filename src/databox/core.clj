(ns databox.core
  (:refer-clojure :exclude [map mapcat filter distinct])
  (:require [clojure.core :as core]
            [clojure.pprint :refer [simple-dispatch]]))

(declare map* mapcat* value failure unbox box? failure? success-value)


(defn- handle-boxed-data
  [boxed-data]
  (case (:type boxed-data)
    :success
    (:result boxed-data)

    :failure
    (throw (ex-info "Unboxed a failed result."
                    (dissoc boxed-data :type :exception)
                    (:exception boxed-data)))))

(defrecord Box
  [type]

  clojure.lang.IDeref
  (deref [box] (handle-boxed-data box))

  clojure.lang.IBlockingDeref
  (deref [box ms timeout-value] (handle-boxed-data box)))

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
           (let [new-value (map* (value v) f options)]
             (rf result new-value))))))

    :else
    (let [obj data
          [f & [options]] args]
      (map* (value obj) f options))))


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
           (let [new-values (mapcat* (value v) f options)]
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
      (mapcat* (value obj) f options))))


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

         (pred (success-value (value input)))
         (rf result (value input))

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
           (let [boxed (value input)
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
           (let [boxed (value input)
                 v (f (success-value boxed))]
             (if (contains? @seen v)
               result
               (do (vswap! seen conj v)
                   (rf result boxed))))))))))

(prefer-method print-method java.util.Map clojure.lang.IDeref)
(prefer-method simple-dispatch java.util.Map clojure.lang.IDeref)

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
  (:result (value v)))

(defn exception
  [v]
  (:exception (value v)))

(defn box?
  [box]
  (instance? Box box))

(defn value
  [v]
  (cond
    (box? v)
    v

    (instance? Throwable v)
    (failure v)

    :else
    (success v)))

(defn unbox
  [v]
  (if (box? v)
    @v
    v))

(defn success?
  [box]
  (= (:type box) :success))

(defn failure?
  [box]
  (= (:type box) :failure))


(defn- stlip-default-keys
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
            r (value (f unboxed))]
        ;; we must keep keys assigned on a original boxed value.
        ;; so remove all default keys of Boxed instance from a original
        ;; boxed value and merge a new boxed instance on it.
        (-> (stlip-default-keys boxed)
            (merge r)
            (map->Box)))

      (catch Throwable th
        (if throw?
          (throw th)
          (-> (stlip-default-keys boxed)
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
            r (value (f unboxed))]
        ;; r is a boxed object containing a coll.
        ;; we must convert it to a coll of boxed objects.
        ;; we can use map* for process the boxed value.
        (let [stlipped (stlip-default-keys boxed)]
          (if (failure? r)
            (-> (merge stlipped r)
                (map->Box))
            (unbox (map* r (fn [coll] (core/map #(-> stlipped
                                                     (merge r)
                                                     (assoc :result %)
                                                     (map->Box))
                                                coll)))))))
      (catch Throwable th
        (if throw?
          (throw th)
          (-> (stlip-default-keys boxed)
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
    `(map (value ~boxed)
          (fn [~v]
            (do ~@body))
          ~opts)))
