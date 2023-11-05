(ns wc.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [wc.core :refer :all]))

(defmacro testing-is-equal
  [description & testcases]
  (let* [expand (fn [{:keys [actual expect]}] `(is (= ~actual ~expect)))
         expanded (map expand testcases)]
        `(testing ~description ~@expanded)))

(deftest make-vector-tests
  (testing-is-equal
   "make-vector tests"
   {:actual (make-vector nil)
    :expect []}
   {:actual (make-vector [])
    :expect []}
   {:actual (make-vector ())
    :expect []}
   {:actual (make-vector #{})
    :expect []}
   {:actual (make-vector {})
    :expect []}
   {:actual (make-vector :a)
    :expect [:a]}
   {:actual (make-vector [:a])
    :expect [:a]}
   {:actual (make-vector [:a :b :c])
    :expect [:a :b :c]}
   {:actual (make-vector '(:a :b :c))
    :expect [:a :b :c]}))

(deftest make-vector-from-map-and-set-tests
  (testing
   "make-vector from map and set tests."
    (is (let [result (make-vector #{:a :b :c})]
          (and (vector? result)
               (= (count result) 3)
               (some #{:a} result)
               (some #{:b} result)
               (some #{:c} result))))
    (is (let [result (make-vector {:a 0 :b 1 :c 2})]
          (and (vector? result)
               (= (count result) 3)
               (some #{[:a 0]} result)
               (some #{[:b 1]} result)
               (some #{[:c 2]} result))))))

(deftest add-arg-tests
  (testing-is-equal
   "add-arg tests"
   {:actual (add-arg {} :a 1)
    :expect {:a [1]}}
   {:actual (add-arg {:a 1} :a 2)
    :expect {:a [1 2]}}
   {:actual (add-arg {:a 1 :b 2} :b 3)
    :expect {:a 1 :b [2 3]}}))

(deftest parse-args-tests
  (testing-is-equal
   "pars-args tests"
   {:actual (parse-args [])
    :expect {}}
   {:actual (parse-args [] {:extra "extra-arg"})
    :expect {:extra "extra-arg"}}
   {:actual (parse-args ["file1"])
    :expect {:filename ["file1"]}}
   {:actual (parse-args ["-c" "file1"])
    :expect {:show [:bytes] :filename ["file1"]}}
   {:actual (parse-args ["--bytes" "file1"])
    :expect {:show [:bytes] :filename ["file1"]}}
   {:actual (parse-args ["-m" "file1"])
    :expect {:show [:chars] :filename ["file1"]}}
   {:actual (parse-args ["--chars" "file1"])
    :expect {:show [:chars] :filename ["file1"]}}
   {:actual (parse-args ["-w" "file1"])
    :expect {:show [:words] :filename ["file1"]}}
   {:actual (parse-args ["--words" "file1"])
    :expect {:show [:words] :filename ["file1"]}}
   {:actual (parse-args ["-l" "file1"])
    :expect {:show [:lines] :filename ["file1"]}}
   {:actual (parse-args ["--lines" "file1"])
    :expect {:show [:lines] :filename ["file1"]}}
   {:actual (parse-args ["-c" "-m" "-w" "file1"])
    :expect {:show [:bytes :chars :words] :filename ["file1"]}}
   {:actual (parse-args ["--chars" "--words" "--lines" "file1"])
    :expect {:show [:chars :words :lines] :filename ["file1"]}}
   {:actual (parse-args ["-c" "-m" "--words" "--lines" "file1" "file2"])
    :expect {:show [:bytes :chars :words :lines]
             :filename ["file1" "file2"]}}))

(deftest get-counts-tests
  (testing-is-equal
   "get-counts tests"
   {:actual (get-counts "")
    :expect {:bytes 0 :chars 0 :words 0 :lines 0}}
   {:actual (get-counts "\n")
    :expect {:bytes 1 :chars 1 :words 0 :lines 1}}
   {:actual (get-counts "abc\n")
    :expect {:bytes 4 :chars 4 :words 1 :lines 1}}
   {:actual (get-counts "abc  def\n")
    :expect {:bytes 9 :chars 9 :words 2 :lines 1}}
   {:actual (get-counts "\n\n")
    :expect {:bytes 2 :chars 2 :words 0 :lines 2}}
   {:actual (get-counts "abc  def\n\n123 456\n")
    :expect {:bytes 18 :chars 18 :words 4 :lines 3}}
   {:actual (get-counts "가나 다라  마바사\n")
    :expect {:bytes 25 :chars 11 :words 3 :lines 1}}))

(deftest get-results-tests
  (testing-is-equal
   "get-results tests"
   {:actual (get-results "가나 다라  마바사\n" [])
    :expect []}
   {:actual (get-results "가나 다라  마바사\n" [:bytes])
    :expect [25]}
   {:actual (get-results "가나 다라  마바사\n" [:chars])
    :expect [11]}
   {:actual (get-results "가나 다라  마바사\n" [:words])
    :expect [3]}
   {:actual (get-results "가나 다라  마바사\n" [:lines])
    :expect [1]}
   {:actual (get-results "가나 다라  마바사\n" [:bytes :words])
     ;; words first and then bytes
    :expect [3 25]}
   {:actual (get-results "가나 다라  마바사\n" [:bytes :words :chars :lines])
     ;; order should be lines words chars and bytes.
    :expect [1 3 11 25]}))
