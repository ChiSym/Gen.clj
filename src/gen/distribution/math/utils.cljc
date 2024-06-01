(ns gen.distribution.math.utils
  "Math utilities and constants.")

(def ^:no-doc ^:const log-pi
  (Math/log Math/PI))

(def ^:no-doc ^:const log-2pi
  (Math/log (* 2 Math/PI)))

(def ^:no-doc ^:const sqrt-2pi
  (Math/sqrt (* 2 Math/PI)))

(def ^:no-doc ^:const half-log-2pi
  (* 0.5 (Math/log (* 2.0 Math/PI))))
