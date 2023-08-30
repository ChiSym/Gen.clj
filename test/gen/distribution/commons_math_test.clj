(ns gen.distribution.commons-math-test
  (:require [clojure.test :refer [deftest]]
            [gen.distribution-test :as dt]
            [gen.distribution.commons-math :as commons]))

(deftest bernoulli-tests
  (dt/bernoulli-tests commons/bernoulli-distribution)
  (dt/bernoulli-gfi-tests commons/bernoulli))

(deftest beta-tests
  (dt/beta-tests commons/beta-distribution))

(deftest uniform-tests
  (dt/uniform-tests commons/uniform-distribution))

(deftest normal-tests
  (dt/normal-tests commons/normal-distribution))

(deftest gamma-tests
  (dt/gamma-tests commons/gamma-distribution))
