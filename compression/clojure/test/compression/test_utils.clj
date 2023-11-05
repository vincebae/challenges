(ns compression.test-utils
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


(defn freq->seq
  "Utility function to build string from frequency table"
  [freq-map]
  (->> freq-map
       (map #(repeat (second %) (first %)))
       (reduce into [])
       (apply str)))


