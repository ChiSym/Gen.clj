(ns gen.distribution.math.gamma
  "Gamma function implementations."
  (:require [gen.distribution.math.utils :as u]))

;; ## Overview
;;
;; The implementation of [[gamma]], [[inv-gamma]], [[log-gamma]],
;; [[log-gamma-1p]], and [[inverse-gamma-1p1]] is adapted from
;; kixi.stats.math (commit 1424567) which was ported from the Apache
;; commons-numbers-gamma library (prior to the Boost implementation in commit
;; 8987d67). These functions have been documented in this namespace with the
;; math and implementation references available in commons-numbers-gamma (since,
;; as of this writing, kixi.stats doesn't include references).

;; ## References
;;
;; Godfrey, P., 2001. A note on the computation of the convergent lanczos
;; complex gamma approximation.
;; https://web.archive.org/web/20201020200358/https://my.fit.edu/~gabdo/gamma.txt
;;
;; Morris, A.H., 1993. NSWC library of mathematics subroutines. Dahlgren
;; Division, Naval Surface Warfare Center.
;; https://apps.dtic.mil/sti/citations/ADA261511
;;
;; NSWC Mathematical Library, 1993. Github.
;; https://github.com/jacobwilliams/nswc
;;
;; Apache Commons Numbers Gamma. Github.
;; https://github.com/apache/commons-numbers-gamma
;;
;; Weisstein, Eric W. Lanczos Approximation. From MathWorld--A Wolfram Web
;; Resource. https://mathworld.wolfram.com/LanczosApproximation.html
;;
;; Weisstein, Eric W. Gamma Function. From MathWorld--A Wolfram Web Resource.
;; https://mathworld.wolfram.com/GammaFunction.html
;;
;; Weisstein, Eric W. Log Gamma Function. From MathWorld--A Wolfram Web
;; Resource. https://mathworld.wolfram.com/LogGammaFunction.html
;;
;; Lanczos, C., 1964. A precision approximation of the gamma function. Journal
;; of the Society for Industrial and Applied Mathematics, Series B: Numerical
;; Analysis, 1(1), pp.86-96.
;; https://epubs.siam.org/doi/abs/10.1137/0701008?journalCode=sjnaam.1
;;
;; DiDonato, A.R. and Morris Jr, A.H., 1986. Computation of the incomplete gamma
;; function ratios and their inverse. ACM Transactions on Mathematical
;; Software (TOMS), 12(4), pp.377-393.
;; https://dl.acm.org/doi/10.1145/22721.23109

(def ^:const LANCZOS_G
  "The Lanczos constant.

  Based on Godfrey's research (Godfrey, P., 2001)."
  (/ 607 128))

(defn lanczos-approximation
  "Computes the Lanczos approximation to the log gamma function for x > 8.

  Based on Wolfram (Weisstein, Eric W., Lanczos Approximation). The coefficients
  were computed by Godfrey (Godfrey, P., 2001). See
  `LanczosApproximation` (Apache Commons Numbers Gamma)."
  ^double
  [^double x]
  {:pre [(> x 8)]}
  (let [c [[14 3.6899182659531625E-6]
           [13 -2.6190838401581408E-5]
           [12 8.441822398385275E-5]
           [11 -1.643181065367639E-4]
           [10 2.1743961811521265E-4]
           [9 -2.1026444172410488E-4]
           [8 1.580887032249125E-4]
           [7 -9.837447530487956E-5]
           [6 4.652362892704858E-5]
           [5 3.399464998481189E-5]
           [4 -0.4919138160976202]
           [3 14.136097974741746]
           [2 -59.59796035547549]
           [1 57.15623566586292]]]
    (reduce (fn [sum [i l]] (+ sum (/ l (+ x i)))) 0.99999999999999709182 c)))

(defn inv-gamma-1pm1
  "Computes an approximation to 1/gamma(1 + x) - 1 for -0.5 <= x <= 1.5.

  Based on the NSWC implementation (Morris, A.H., 1993). See the `DGAM1`
  function (NSWC Mathematical Library, 1993) which defines the coefficients of
  the Maclaurin series expansion (A, B, C, P, Q). See also `InvGamma1pm` (Apache
  Commons Numbers Gamma)."
  ^double
  [^double x]
  {:pre [(>= x -0.5)
         (<= x 1.5)]}
  (let [t (if (<= x 0.5) x (- (- x 0.5) 0.5))
        C [-0.205633841697760710345015413002057E-06
           0.113302723198169588237412962033074E-05
           -0.125049348214267065734535947383309E-05
           -0.201348547807882386556893914210218E-04
           0.128050282388116186153198626328164E-03
           -0.215241674114950972815729963053648E-03
           -0.116516759185906511211397108401839E-02
           0.721894324666309954239501034044657E-02
           -0.962197152787697356211492167234820E-02
           -0.421977345555443367482083012891874E-01
           0.166538611382291489501700795102105E+00
           -0.420026350340952355290039348754298E-01
           -0.655878071520253881077019515145390E+00]]
    (if (neg? t)
      (let [A [0.611609510448141581788E-08
               0.624730830116465516210E-08]
            B [0.195755836614639731882E-09
               -0.607761895722825260739E-07
               0.992641840672773722196E-06
               -0.643045481779353022248E-05
               -0.851419432440314906588E-05
               0.493944979382446875238E-03
               0.266205348428949217746E-01
               0.203610414066806987300E+00]
            CA -0.422784335098467139393487909917598E+00
            [a0 a1] A
            b (inc (* t (reduce (fn [b b'] (+ (* t b) b')) B)))
            c (+ CA (* t (reduce (fn [c c'] (+ (* t c) c')) (/ (+ a0 (* a1 t)) b) C)))]
        (if (> x 0.5)
          (* t (/ c x))
          (* x (inc c))))
      (let [P [4.343529937408594E-15
               -1.2494415722763663E-13
               1.5728330277104463E-12
               4.686843322948848E-11
               6.820161668496171E-10
               6.8716741130671986E-9
               6.116095104481416E-9]
            Q [2.6923694661863613E-4
               0.004956830093825887
               0.054642130860422966
               0.3056961078365221]
            CB 0.577215664901532860606512090082402E+00
            p (reduce (fn [p p'] (+ (* t p) p')) P)
            q (inc (* t (reduce (fn [q q'] (+ (* t q) q')) Q)))
            c (+ CB (* t (reduce (fn [c c'] (+ (* t c) c')) (/ p q) C)))]
        (if (> x 0.5)
          (* (/ t x) (dec c))
          (* x c))))))

(defn log-gamma-1p
  "Computes an approximation to the natural logarithm of gamma(1 + x) for -0.5 <=
  x <= 1.5.

  Based on the NSWC implementation (Morris, A.H., 1993). See the `DGMLN1`
  function (NSWC Mathematical Library, 1993) and `LogGamma1p` (Apache Commons
  Numbers Gamma)."
  ^double
  [^double x]
  {:pre [(>= x -0.5)
         (<= x 1.5)]}
  (- (Math/log1p (inv-gamma-1pm1 x))))

(defn log-gamma
  "Computes an approximation to the natural logarithm of the gamma function for
  positive x.

  Based on Wolfram (Weisstein, Eric W., Log Gamma Function). For x <= 8, follows
  the NSWC implementation (Morris, A.H., 1993). See the `DGAMLN` function (NSWC
  Mathematical Library, 1993). For x > 8, follows the Lanczos approximation
  method. See `LogGamma` (Apache Commons Numbers Gamma)."
  ^double
  [^double x]
  {:pre [(pos? x)]}
  (cond
    (< x 0.5) (- (log-gamma-1p x) (Math/log x))
    (<= x 2.5) (log-gamma-1p (dec x))
    (<= x 8.0) (let [n (int (Math/floor (- x 1.5)))]
                 (+ (log-gamma-1p (- x (inc n)))
                    (loop [i 1
                           p 1.0]
                      (if (<= i n)
                        (recur (inc i) (* p (- x i)))
                        (Math/log p)))))
    :else (let [t (+ x LANCZOS_G 0.5)]
            (+ (- (* (+ x 0.5) (Math/log t)) t)
               u/half-log-2pi
               (Math/log (/ (lanczos-approximation x) x))))))

(defn gamma
  "Computes an approximation to the gamma function for negative and positive x.

  Based on Wolfram (Weisstein, Eric W., Gamma Function). Follows the NSWC
  implementation (Morris, A.H., 1993). See the `DGAMMA` function (NSWC
  Mathematical Library, 1993) and `Gamma` (Apache Commons Numbers Gamma)."
  ^double
  [^double x]
  (let [abs-x (abs x)]
    (if (<= abs-x 20)
      (if (>= x 1)
        (loop [t (dec x) p 1]
          (if (> t 1.5)
            (recur (dec t) (* p t))
            (/ p (inc (inv-gamma-1pm1 t)))))
        (loop [t (inc x) p x]
          (if (< t 0.5)
            (recur (inc t) (* p t))
            (/ 1 (* p (inc (inv-gamma-1pm1 (dec t))))))))
      (let [y (+ abs-x LANCZOS_G 0.5)
            abs-g (* (/ u/sqrt-2pi abs-x)
                     (Math/pow y (+ abs-x 0.5))
                     (Math/exp (- y))
                     (lanczos-approximation abs-x))]
        (if (pos? x)
          abs-g
          (/ (- Math/PI)
             (* x abs-g (Math/sin (* Math/PI x)))))))))
