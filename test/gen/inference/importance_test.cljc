(ns gen.inference.importance-test
  (:require [clojure.test :refer [deftest is testing]]
            [gen.choicemap :as choicemap :refer [choicemap get-value]]
            [gen.distribution.kixi :as dist]
            [gen.dynamic :as dynamic :refer [gen]]
            [gen.inference.importance :as importance]
            [gen.trace :as trace]))

(def model-causing-rejection-sampling
  (gen
    []
    (if (dynamic/trace! :foo dist/bernoulli 0.5)
      (dynamic/trace! :bar dist/bernoulli 1.0)
      (dynamic/trace! :bar dist/bernoulli 0.0))))

(deftest rejection
  (testing "Robustness in the presence of importance samples with weight log(0)."
    (is {:foo true :bar true}
        ;; Needs a couple of samples to trigger previous bug here.
        (-> (importance/resampling model-causing-rejection-sampling [] (choicemap {:bar true}) 10)
            (:trace)
            (trace/get-choices)
            (get-value)))))
