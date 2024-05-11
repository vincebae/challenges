(ns json-parser.core-test
  (:require
    [clojure.test :refer [deftest is]]
    [json-parser.core :refer [json->edn]]
    [json-parser.test-utils :refer [expand-test-cases]]))


(defn- build-test-filenames
  [testcases]
  (let
    [base-dir "./test/json_parser/testfiles/"]
    (loop [[x & xs] testcases, test-files []]
      (if-let [[test-dir & filenames] (seq x)]
        (recur xs (into test-files (map #(str base-dir test-dir %) filenames)))
        test-files))))


(defmacro testing-valid-files
  [desc & testcases]
  (list `expand-test-cases desc (build-test-filenames testcases)
        (fn [filename#]
          (let [result-file# (str filename# ".expected")
                expected# (read-string (slurp result-file#))]
            `(is (= (json->edn ~filename#) ~expected#))))))


(defmacro testing-invalid-files
  [desc & testcases]
  (list `expand-test-cases desc (build-test-filenames testcases)
        (fn [filename#]
          `(is (let [~'result# (json->edn ~filename#)]
                 (or (nil? ~'result#)
                     (= (first ~'result#) :error)))))))


(deftest valid-files-tests
  (testing-valid-files
    "test cases for valid json files"
    ["step1/" "valid.json"]
    ["step2/" "valid.json" "valid2.json"]
    ["step3/" "valid.json"]
    ["step4/" "valid.json" "valid2.json"]))


(deftest invalid-files-tests
  (testing-invalid-files
    "test cases for invalid json files"
    ["step1/" "invalid.json"]
    ["step2/" "invalid.json" "invalid2.json"]
    ["step3/" "invalid.json"]
    ["step4/" "invalid.json"]))
