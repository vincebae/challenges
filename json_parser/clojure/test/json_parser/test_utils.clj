;; https://codingchallenges.fyi/challenges/challenge-json-parser/

(ns json-parser.test-utils
  (:require
    [clojure.test :refer [testing is]]))


;; macros for testing
(defmacro expand-test-cases
  [desc cases expand-fn]
  (let [expanded# (map expand-fn cases)]
    `(testing ~desc ~@expanded#)))


(defmacro testing-is-equal
  [desc & cases]
  (list `expand-test-cases desc cases
        (fn [{actual# :actual expect# :expect}]
          `(is (= ~actual# ~expect#)))))


