(ns wc.int-test
  (:require
   [clojure.java.shell :as sh]
   [clojure.test :refer [deftest is testing]]))

(defmacro testing-wc-output
  [description & testcases]
  (let*
   [expand (fn [{:keys [params expect]}]
             `(is (= (:out (sh/sh "bash" "-c" (str ~params)))
                     ~expect)))
    expanded (map expand testcases)]
   `(testing ~description ~@expanded)))

(deftest main-tests
  (testing-wc-output
   "main-tests"
   {:params "./wc.clj test/wc/testfiles/empty.txt"
    :expect "0 0 0 test/wc/testfiles/empty.txt\n"}
   {:params "./wc.clj test/wc/testfiles/space.txt"
    :expect "1 0 2 test/wc/testfiles/space.txt\n"}
   {:params "./wc.clj test/wc/testfiles/test_file*"
    :expect (str "2 3 28 test/wc/testfiles/test_file1.txt\n"
                 "1 3 21 test/wc/testfiles/test_file2.txt\n"
                 "4 7 42 test/wc/testfiles/test_file3.txt\n")}
   ;; Output order should be words and bytes.
   {:params "./wc.clj -c -w test/wc/testfiles/test_file1.txt"
    :expect "3 28 test/wc/testfiles/test_file1.txt\n"}
   ;; Output order should be lines and chars.
   {:params "./wc.clj --chars --lines test/wc/testfiles/test_file1.txt"
    :expect "2 18 test/wc/testfiles/test_file1.txt\n"}
   ;; Output order should be lines words chars and bytes.
   {:params "./wc.clj --bytes -m --words -l test/wc/testfiles/test_file1.txt"
    :expect "2 3 18 28 test/wc/testfiles/test_file1.txt\n"}
   {:params "cat test/wc/testfiles/test_file1.txt | ./wc.clj --bytes -m --words -l"
    :expect "2 3 18 28\n"}
   ;; No file found
   {:params "./wc.clj invalid_file"
    :expect "invalid_file (No such file or directory)\n"}))
