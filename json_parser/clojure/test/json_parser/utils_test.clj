(ns json-parser.utils-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [json-parser.utils :refer [error]]))

(deftest error-tests
  (testing
    "test cases for error"
    (is (= (error "msg") [:error "msg"]))
    (is (= (error "msg" 123 \a) [:error "msg123a"]))))


