(ns gen.distribution.math.gamma-benchmark
  (:require [clojure.test :refer [deftest is testing]]
            [same.core :refer [ish? with-comparator]]
            [gen.generators :refer [within]]
            [gen.distribution.math.gamma :as g]
            [criterium.core :as crit]))

(def ^:no-doc ^:const lanczos-approximation-baseline
  [{:n 15
    :x 9
    :mean 3.127368933269932E-6
    :variance 1.5780434242573877E-14
    :lower-q 2.984515105404977E-6
    :upper-q 3.320172412426692E-6}
   {:n 15
    :x 100
    :mean 2.0160584455226908E-7
    :variance 7.928671640576774E-18
    :lower-q 1.9907849968797706E-7
    :upper-q 2.0489675953499433E-7}
   {:n 15
    :x 170
    :mean 2.040524636004644E-7
    :variance 5.795520133553684E-18
    :lower-q 2.0120967339244923E-7
    :upper-q 2.0719692107157572E-7}])

(deftest ^:benchmark lanczos-approximation-benchmark
  []
  (let [f (fn[b]
            (let [n (:n b)
                  x (:x b)
                  mean (:mean b)
                  variance (:variance b)
                  lower-q (:lower-q b)
                  upper-q (:upper-q b)
                  stats (crit/quick-benchmark
                         (dotimes [_ n]
                           (g/lanczos-approximation x)) {})]
              (testing "against lanczos approximation baseline"
                (with-comparator (within 1e-3)
                  (is (ish? mean (first (:mean stats))) x)
                  (is (ish? variance (first (:variance stats))) x)
                  (is (ish? lower-q (first (:lower-q stats))) x)
                  (is (ish? upper-q (first (:upper-q stats))) x)))))]
    (run! f lanczos-approximation-baseline)))

(deftest ^:benchmark inv-gamma-1pm1-benchmark
  []
  (let [n 15
        x [-0.05 1.5]
        f (fn[x]
            (println "inv-gamma-1pm1: " x)
            (crit/quick-bench
                (dotimes [_ n]
                  (g/inv-gamma-1pm1 x))))]
    (run! f x)))

(deftest ^:benchmark log-gamma-1p-benchmark
  []
  (let [n 15
        x [-0.05 1.5]
        f (fn[x]
            (println "log-gamma-1p: " x)
            (crit/quick-bench
                (dotimes [_ n]
                  (g/log-gamma-1p x))))]
    (run! f x)))


(deftest ^:benchmark log-gamma-benchmark
  []
  (let [n 15
        x [0.1 1.5 2.5 8.0 10.0]
        f (fn[x]
            (println "log-gamma: " x)
            (crit/quick-bench
                   (dotimes [_ n]
                     (g/log-gamma x))))]
    (run! f x)))

(deftest ^:benchmark gamma-benchmark
  []
  (let [n 15
        x [-0.001 0.1 1.5 2.5 8.0 10.0]
        f (fn[x]
            (println "gamma: " x)
            (crit/quick-bench
                (dotimes [_ n]
                  (g/gamma x))))]
    (run! f x)))
