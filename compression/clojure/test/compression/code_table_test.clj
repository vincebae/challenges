(ns compression.code-table-test
  (:require
    [clojure.test :refer [deftest]]
    [compression.code-table :as ct]
    [compression.huffman :as h]
    [compression.test-utils :refer [testing-is-equal freq->seq]]))


(defn- build-table-bit-vector
  [input-seq]
  (-> input-seq
      h/build-tree
      (ct/build-encode-table :bit-vector)))


(defn- build-table-code-map
  [input-seq]
  (-> input-seq
      h/build-tree
      ct/build-encode-table))


(deftest build-table-bit-vector-test
  (testing-is-equal
    "test case for building huffman code table"
    {:actual (build-table-bit-vector "")
     :expect {}}
    {:actual (build-table-bit-vector "a")
     :expect {\a [0]}}
    {:actual (build-table-bit-vector "aba")
     :expect {\b [0]
              \a [1]}}
    {:actual (build-table-bit-vector "aabbaca")
     :expect {\c [0 0]
              \b [0 1]
              \a [1]}}
    {:actual (build-table-bit-vector
               (freq->seq {\c 32, \d 42, \e 120, \k 7, \l 43, \m 24, \u 37, \z 2}))
     :expect {\c [1 1 1 0]
              \d [1 0 1]
              \e [0]
              \k [1 1 1 1 0 1]
              \l [1 1 0]
              \m [1 1 1 1 1]
              \u [1 0 0]
              \z [1 1 1 1 0 0]}}))


(deftest build-table-code-map-test
  (testing-is-equal
    "test case for building huffman code table"
    {:actual (build-table-code-map "")
     :expect {}}
    {:actual (build-table-code-map "a")
     :expect {\a {:length 1 :code 0}}}
    {:actual (build-table-code-map "aba")
     :expect {\b {:length 1 :code 0}
              \a {:length 1 :code 1}}}
    {:actual (build-table-code-map "aabbaca")
     :expect {\a {:length 1 :code 1}
              \c {:length 2 :code 2r00}
              \b {:length 2 :code 2r01}}}
    {:actual (build-table-code-map
               (freq->seq {\c 32, \d 42, \e 120, \k 7, \l 43, \m 24, \u 37, \z 2}))
     :expect {\c {:length 4 :code 2r1110}
              \d {:length 3 :code 2r101}
              \e {:length 1 :code 0}
              \k {:length 6 :code 2r111101}
              \l {:length 3 :code 2r110}
              \m {:length 5 :code 2r11111}
              \u {:length 3 :code 2r100}
              \z {:length 6 :code 2r111100}}}))
