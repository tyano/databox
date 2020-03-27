# databox

A Clojure library for boxing values or exceptions for passing data through channels of core.async.

## Build

The latest version on Clojars.

[![Clojars Project](https://img.shields.io/clojars/v/databox.svg)](https://clojars.org/databox)

## Usage

You can wrap any value with `box/value`, and transform it's value with `box/map`, `box/filter`, `box/mapcat`. 
A boxed value is derefable by `deref` or `@`.

```clojure
(require '[databox.core :as box])
(let [boxed (box/value :a)]
  (-> boxed
      (box/map name)
      (box/map string/upper-case)
      (deref)))
;; A
```

`box/value` never be nested. If you call `box/value` on a already boxed data, `box/value` returns the data unchanged.

```clojure
(let [boxed (box/value :a)]
  (= boxed (box/value boxed))) ; => true
```

If you wrap a throwable object with `box/value`, the box become a `box/failure` and will throw the contained throwable when it's dereference.

```clojure
(let [boxed (box/value (ex-info "error" {}))]
  @boxed) ;; <-- throw an exception
```

All transformers like `box/map` will convert a normal value to boxed value automatically, so you can pass any value to the transformers in safe.

```clojure
(let [data (box/map name :a)]
  (assert (true? (box/box? data)))
  (assert (= "A" @data)))
```

Transformers are transducers.

```clojure
(doseq [boxed (sequence (box/map name) [(box/value :a) (box/value :b) (box/value :c)])]
  (println @boxed))

;; A
;; B
;; C
```

All transformers ignore failure-boxed data. You don't need to care that if sequences or channels contains error data. Just transform them with box-transformers.

with sequence:

```clojure
(let [data-call [(box/value :a) (box/failure (ex-info "error" {})) (box/value :c)]]
      converted (sequence (box/map name) data-coll)] ;; No exception because failure-box is ignored by box/map
  (doseq [boxed converted]
    (println @boxed))) ;; will thow an exception when the second item is unwrapped.
```

with channels:

```clojure
(let [ch (pipe my-channel
               (chan 1 (box/map name)))] ;; You can connect a tranformer as a transducer to a channel.
  (go
    ;; Here we send a (boxed) throwable to a channel and it will be transformed by `box/map`, 
    ;; But `box/map` ignores all error data so exception never be thrown. 
    ;; We can apply box transformers on channel pipelines in safe.
    (onto-chan my-channel [(box/value :a) (box/value (ex-info "error" {})) (box/value :c)]))
    
  (go
    (let [data (<! ch)]
      ;; all exceptions will be thrown when it derefed.
      ;; so all errors in channel pipelines never be thrown IN pipeline, 
      ;; but be thrown at the END of pipeline. 
      (println @data))))
```

You can not only convert a data by box/map, but can do filter, mapcat and distinct them.

```clojure
(let [ch (-> mychannel
             ;; string/split returns a seq of string and the each string will be boxed by `box/mapcat`
             (pipe (chan 1 (box/mapcat #(string/split % ",")))) 
             ;; all "a" are removed
             (pipe (chan 1 (box/filter #(not= "a"))))
             ;; all duplicated items will be removed
             (pipe (chan 1 (box/distinct))))]
  (go
    (>! mychannel (box/value "a,b,a,c,c,b,d")))
    
  (go
    (let [item (<! ch)]
      (println item))))
      
;; b
;; c
;; d
```

With `maplet`, you can safely unwrap boxed values and process them. But currently only one value is supported.
maplet is a syntax sugar of `box/map`.

```clojure
(maplet [v boxed-value]
  (name v))
```

If you don't certain it a value is boxed or normal value, you can deref it with `box/unbox` in safe.
`box/unbox` returns an unboxed (derefed) value or the value itself if the value is not boxed. 

```clojure
(box/unbox :a)

;; :a
```

You can check if a boxed value contains an exception or not by `box/failure?` or `box/success?`.
if a value contains a throwable, `box/failure?` returns true.

```clojure
(let [boxed (boxed/failure (ex-info "error" {}))]
  (box/failure? boxed) ;=> true
  (box/success? boxed) ;=> false
)
```

If you need the exception wrapped in a boxed data, you can use `box/exception`.

```clojure
(let [boxed (boxed/failure (ex-info "error" {}))
      ex (box/exception boxed)]
  (ex-data ex))
```

`box/box?` return true if a data is boxed data.

```clojure
(box/box? (box/value :a)) ; => true
(box/box? :a) ; => false
```


## License

Copyright © 2019 YANO Tsutomu

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
