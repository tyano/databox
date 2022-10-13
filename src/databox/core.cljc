(ns databox.core
  (:refer-clojure :exclude [map mapcat filter distinct Box ->Box apply])
  (:require #?(:clj  [clojure.core :as core]
               :cljs [cljs.core :as core])
            #?(:clj [clojure.pprint :as pp])))

(declare map* mapcat* box failure unbox box? success? failure? success-value exception)

(defn- handle-boxed-data
  [boxed-data]
  (case (:type boxed-data)
    :success
    (:result boxed-data)

    :failure
    (throw (:exception boxed-data))))

#?(:clj
   (defrecord Box [type]
     clojure.lang.IDeref
     (deref [box] (handle-boxed-data box))

     clojure.lang.IBlockingDeref
     (deref [box ms timeout-value] (handle-boxed-data box)))
   
   :cljs
   (defrecord Box [type]
     IDeref
     (-deref [box] (handle-boxed-data box))))

#?(:clj
   (defmethod print-method Box [v ^java.io.Writer w]
     (if (success? v)
       (.write w (str "Success[" (pr-str (success-value v)) "]"))
       (.write w (str "Failure[" (pr-str (exception v)) "]")))))

#?(:clj
   (prefer-method pp/simple-dispatch clojure.lang.IPersistentMap clojure.lang.IDeref))

(defn disable
  [boxed]
  (assoc (box boxed) ::disabled true))

(defn enable
  [boxed]
  (dissoc (box boxed) ::disabled))

(defn disabled?
  [boxed]
  (true? (::disabled boxed)))

(defn apply
  [v f & {failure-value :failure :or {failure-value nil}}]
  (let [boxed (box v)]
    (cond
      (failure? boxed)
      failure-value

      (disabled? boxed)
      (success-value boxed)

      :else
      (f (success-value boxed)))))

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
           (let [boxed (box v)
                 new-value (if (disabled? boxed)
                             boxed
                             (map* boxed f options))]
             (rf result new-value))))))

    :else
    (let [obj data
          [f & [options]] args
          boxed (box obj)]
      (if (disabled? boxed)
        boxed
        (map* boxed f options)))))


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
           (let [boxed (box v)
                 new-values (if (disabled? boxed)
                              [boxed]
                              (mapcat* boxed f options))]
             (reduce
              #(let [ret (rf %1 %2)]
                 (if (reduced? ret)
                   (reduced ret)
                   ret))
              result
              new-values))))))

    :else
    (let [obj data
          [f & [options]] args
          boxed (box obj)]
      (if (disabled? boxed)
        boxed
        (mapcat* boxed f options)))))


(defn filter
  "filter transducer for filterling boxed data by the contained value."
  [pred]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (let [boxed (box input)]
         (cond
           (failure? boxed)
           (rf result boxed)

           (disabled? boxed)
           (rf result boxed)

           (pred (success-value boxed))
           (rf result boxed)

           :else
           result))))))

(defn distinct
  []
  (fn [rf]
    (let [seen (volatile! #{})]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [boxed (box input)]
           (cond
             (failure? boxed)
             (rf result boxed)

             (disabled? boxed)
             (rf result boxed)

             :else
             (let [v (success-value boxed)]
               (if (contains? @seen v)
                 result
                 (do (vswap! seen conj v)
                     (rf result boxed)))))))))))

(defn distinct-by
  [f]
  (fn [rf]
    (let [seen (volatile! #{})]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [boxed (box input)]
           (cond
             (failure? boxed)
             (rf result boxed)

             (disabled? boxed)
             (rf result boxed)

             :else
             (let [v (f (success-value boxed))]
               (if (contains? @seen v)
                 result
                 (do (vswap! seen conj v)
                     (rf result boxed)))))))))))

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
  #?(:clj
     (cond
       (box? v)
       v

       (instance? Throwable v)
       (failure v)

       :else
       (success v))
     
     :cljs
     (cond
       (box? v)
       v

       :else
       (success v))))

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
        ;; So copy all values of a new boxed data to the original box.
        ;; But remove all default keys of Boxed instance from a original box
        ;; before merging (aka initialize).
        (-> (strip-default-keys boxed)
            (merge r)
            (map->Box)))

      (catch #?(:clj Throwable :cljs :default) th
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
      ;; r is a boxed object containing a coll.
      ;; we must convert it to a coll of boxed objects.
      ;; we can use map* for processing the boxed value.
      (let [unboxed (unbox boxed)
            r (box (f unboxed))
            stripped (strip-default-keys boxed)]
        (if (failure? r)
          (-> (merge stripped r)
              (map->Box))
          (unbox (map* r (fn [coll] (core/map #(-> stripped
                                                   (merge r)
                                                   (assoc :result %)
                                                   (map->Box))
                                              coll))))))
      (catch #?(:clj Throwable :cljs :default) th
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
