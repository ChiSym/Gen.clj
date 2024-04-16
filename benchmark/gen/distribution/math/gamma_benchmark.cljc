(ns gen.distribution.math.gamma-benchmark
  (:require [gen.distribution.math.gamma :as g]
            [criterium.core :as crit]))

(defn ^:benchmark lanczos-approximation-computation-time
  []
  (let [n 15
        x [9 100 170]
        f (fn[x]
            (println "lanczos-approximation: " x)
            (crit/quick-bench
                (dotimes [_ n]
                  (g/lanczos-approximation x))))]
    (mapv f x)))

(defn ^:benchmark inv-gamma-1pm1-computation-time
  []
  (let [n 15
        x [-0.05 1.5]
        f (fn[x]
            (println "inv-gamma-1pm1: " x)
            (crit/quick-bench
                (dotimes [_ n]
                  (g/inv-gamma-1pm1 x))))]
    (mapv f x)))

(defn ^:benchmark log-gamma-1p-computation-time
  []
  (let [n 15
        x [-0.05 1.5]
        f (fn[x]
            (println "log-gamma-1p: " x)
            (crit/quick-bench
                (dotimes [_ n]
                  (g/log-gamma-1p x))))]
    (mapv f x)))


(defn ^:benchmark log-gamma-computation-time
  []
  (let [n 15
        x [0.1 1.5 2.5 8.0 10.0]
        f (fn[x]
            (println "log-gamma: " x)
            (crit/quick-bench
                   (dotimes [_ n]
                     (g/log-gamma x))))]
    (mapv f x)))

(defn ^:benchmark gamma-computation-time
  []
  (let [n 15
        x [-0.001 0.1 1.5 2.5 8.0 10.0]
        f (fn[x]
            (println "gamma: " x)
            (crit/quick-bench
                (dotimes [_ n]
                  (g/gamma x))))]
    (mapv f x)))
