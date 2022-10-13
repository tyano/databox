(ns databox.core-test
  (:require [clojure.test :refer :all]
            [databox.core :as box]
            [clojure.string :refer [upper-case]]))

(deftest map-test
  (testing "mapping"
    (is (= (box/success "a")
           (box/map (box/success :a) name))))

  (testing "ignored if the box is disabled (not transducer)"
    (is (= (box/disable :a)
           (box/map (box/disable :a) name))))

  (testing "if map is applied on a no-boxed value, the result must be a boxed value."
    (is (= (box/success "a")
           (box/map :a name))))

  (testing "map can be applied through threading macro."
    (are [v] (= (box/success "A") (-> v (box/map name) (box/map upper-case)))
      (box/success :a)
      :a))

  (testing "if an exception occurred through threading map calls, the last result becomes a failure value containing the exception."
    (is (box/failure? (-> (box/success :a)
                          (box/map name)
                          (box/map (fn [_] (throw (ex-info "test error" {}))))
                          (box/map upper-case)))))

  (testing "actions after an exception never be processed."
    (let [not-eval (atom nil)]
      (is (nil?
           (do (-> (box/success :a)
                   (box/map name)
                   (box/map (fn [_] (throw (ex-info "test error" {}))))
                   (box/map (fn [v] (reset! not-eval :error) (upper-case v))))
               @not-eval)))))

  (testing "all keys on a boxed item must remain after a mapping process."
    (is (= {:transaction-id :test}
           (let [r (box/map (assoc (box/success :a)
                                   :transaction-id :test)
                            name)]
             (dissoc r :result :type)))))

  (testing "if a result is a failure item, the failure item must contains all keys which an original boxed item have."
    (let [ex (ex-info "test error" {})
          expected (assoc (box/failure ex) :transaction-id :id)]
      (is (= expected
             (-> (box/success :a)
                 (assoc :transaction-id :id)
                 (box/map name)
                 (box/map (fn [_] (throw ex)))
                 (box/map upper-case))))))

  (testing "Can make a transducer by map."
    (is (= [(box/success "A")
            (box/success "B")
            (box/success "C")]
           (sequence (comp (box/map name)
                           (box/map upper-case)) [:a :b :c]))))
  
  (testing "ignored if the box is disabled (map transducer)"
    (is (= [(box/disable :a)
            (box/disable :b)
            (box/disable :c)]
           (sequence (comp (box/map name)
                           (box/map upper-case)) [(box/disable :a) (box/disable :b) (box/disable :c)])))))

(deftest mapcat-test
  (testing "mapcat"
    (is (= [(box/success \a)
            (box/success \b)
            (box/success \c)]
           (box/mapcat (box/success "abc") seq))))
  
  (testing "ignored if the box is disabled"
    (is (= (box/disable "abc")
           (box/mapcat (box/disable "abc") seq))))

  (testing "if mapcat is applied on a no-boxed value, the result must be a boxed value."
    (is (= [(box/success \a)
            (box/success \b)
            (box/success \c)]
           (box/mapcat "abc" seq))))

  (testing "if an exception occurred through threading calls, the last result becomes a failure value containing the exception."
    (is (box/failure? (-> (box/success "abc")
                          (box/mapcat seq)
                          (box/map (fn [_] (throw (ex-info "test error" {}))))
                          (box/map upper-case)))))

  (testing "actions after an exception never be processed."
    (let [not-eval (atom nil)]
      (is (nil?
           (let [data-coll (-> (box/success "abc") (box/mapcat seq))]
             (dorun (->> data-coll
                         (map #(box/map % (fn [_] (throw (ex-info "test error" {})))))
                         (map #(box/map % (fn [v] (reset! not-eval :error) (upper-case (str v)))))))
             @not-eval)))))

  (testing "all keys on a boxed item must remain after a mapcat process."
    (is (= [(assoc (box/success \a) :transaction-id :test)
            (assoc (box/success \b) :transaction-id :test)
            (assoc (box/success \c) :transaction-id :test)]
           (box/mapcat (assoc (box/success "abc") :transaction-id :test) seq))))

  (testing "if a result of mapcat processes is a failure item, the failure item must contains all keys which an original boxed item have."
    (let [ex (ex-info "test error" {})
          data (assoc (box/failure ex) :transaction-id :id)
          expected [data data data]]
      (is (= expected
             (let [data-coll (-> (assoc (box/success "abc") :transaction-id :id) (box/mapcat seq))]
               (->> data-coll
                    (map #(box/map % (fn [_] (throw ex))))))))))

  (testing "Can make a transducer by mapcat."
    (is (= [(box/success "A")
            (box/success "B")
            (box/success "C")
            (box/success "D")
            (box/success "E")
            (box/success "F")
            (box/success "G")
            (box/success "H")
            (box/success "I")]
           (sequence (comp (box/mapcat seq)
                           (box/map str)
                           (box/map upper-case)) ["abc" "def" "ghi"]))))
  
  (testing "ignored it the box is disabled (mapcat transducer)"
    (is (= [(box/disable "abc")
            (box/disable "def")
            (box/disable "ghi")]
           (sequence (comp (box/mapcat seq)
                           (box/map str)
                           (box/map upper-case)) [(box/disable "abc") (box/disable "def") (box/disable "ghi")]))))

  (testing "Can filter boxed items by the contained values."
    (is (= [(box/success 1) (box/success 3) (box/success 5)]
           (sequence (box/filter odd?) [1 2 3 4 5])))

    (is (= [(box/success :a) (box/success :b)]
           (sequence (box/filter some?) [(box/success :a)
                                         (box/success nil)
                                         (box/success :b)]))))
  
  (testing "Ignored if the box is disabled (filter transducer)"
    (is (= [(box/disable 1) (box/disable 2) (box/disable 3) (box/disable 4) (box/disable 5)]
           (sequence (box/filter odd?) (mapv box/disable [1 2 3 4 5]))))

    (is (= [(box/disable :a) (box/disable nil) (box/disable :b)]
           (sequence (box/filter some?) [(box/disable :a)
                                         (box/disable nil)
                                         (box/disable :b)])))))


(deftest distinct-test
  (testing "duplicated data must be removed"
    (is (= [(box/success 1) (box/success 2) (box/success 3)]
           (sequence (box/distinct) [1 1 1 2 1 3 2 3 1 2]))))
  
  (testing "ignored if the box is disabled"
    (is (= [(box/disable 1)
            (box/disable 1)
            (box/disable 1)
            (box/disable 2)
            (box/disable 1)
            (box/disable 3)
            (box/disable 2)
            (box/disable 3)
            (box/disable 1)
            (box/disable 2)]
           (sequence (box/distinct) (mapv box/disable [1 1 1 2 1 3 2 3 1 2]))))))

(deftest distinct-by-test
  (testing "data must be removed if the result of f is duplicated."
    (is (= [(box/success {:data 1 :type :one})
            (box/success {:data 1 :type :two})
            (box/success {:data 3 :type :three})]
           (sequence (box/distinct-by :type)
                     [{:data 1 :type :one}
                      {:data 2 :type :one}
                      {:data 3 :type :one}
                      {:data 1 :type :two}
                      {:data 2 :type :two}
                      {:data 3 :type :one}
                      {:data 3 :type :two}
                      {:data 3 :type :three}]))))
  
  (testing "ignored if the box is disabled"
    (is (= [(box/disable {:data 1 :type :one})
            (box/disable {:data 2 :type :one})
            (box/disable {:data 3 :type :one})
            (box/disable {:data 1 :type :two})
            (box/disable {:data 2 :type :two})
            (box/disable {:data 3 :type :one})
            (box/disable {:data 3 :type :two})
            (box/disable {:data 3 :type :three})]
           (sequence (box/distinct-by :type)
                     (mapv box/disable
                           [{:data 1 :type :one}
                            {:data 2 :type :one}
                            {:data 3 :type :one}
                            {:data 1 :type :two}
                            {:data 2 :type :two}
                            {:data 3 :type :one}
                            {:data 3 :type :two}
                            {:data 3 :type :three}]))))))
