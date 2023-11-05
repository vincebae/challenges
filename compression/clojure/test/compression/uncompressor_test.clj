(ns compression.uncompressor-test
  (:require
    [clojure.test :refer [deftest]]
    [compression.test-utils :refer [testing-is-equal]]
    [compression.uncompressor :as u]))


(def ub unchecked-byte)


(def decode-table-bit-vector
  {:code-type :bit-vector
   [1 1 1 1 0 0] \z,
   [1 1 1 1 0 1] \k,
   [1 0 0] \u,
   [1 1 1 1 1] \m,
   [0] \e,
   [1 1 0] \l,
   [1 1 1 0] \c,
   [1 0 1] \d})


(def decode-table-code-map
  {:code-type :code-map
   1 {0 \e},
   3 {2r101 \d,
      2r110 \l,
      2r100 \u},
   4 {2r1110 \c},
   5 {2r11111 \m},
   6 {2r111101 \k,
      2r111100 \z}})


(def huffman-tree
  {:weight 307
   :lchild {:value \e :weight 120}
   :rchild {:weight 187
            :lchild  {:weight 79
                      :lchild    {:value \u :weight 37}
                      :rchild {:value \d :weight 42}}
            :rchild {:weight 108
                     :lchild {:value \l :weight 43}
                     :rchild {:weight 65
                              :lchild {:value \c :weight 32}
                              :rchild {:weight 33
                                       :lchild {:weight 9
                                                :lchild {:value \z :weight 2}
                                                :rchild {:value \k :weight 7}}
                                       :rchild {:value \m :weight 24}}}}}})


(deftest decode-bit-vector-tests
  (testing-is-equal
    "test cases for decode with bit-vector decode table"
    {:actual (u/decode decode-table-bit-vector [(ub 2r11101010)] 3)
     :expect (seq "cde")}
    {:actual (u/decode decode-table-bit-vector [(ub 2r00111101) (ub 2r11011010) (ub 2r0)] 6)
     :expect (seq "eekllu")}
    {:actual (u/decode decode-table-bit-vector [(ub 2r00111101) (ub 2r11011010) (ub 2r0)] 5)
     :expect (seq "eekll")}
    {:actual (u/decode decode-table-bit-vector [] 1)
     :expect '()}
    ;;
    ))


(deftest decode-code-map-tests
  (testing-is-equal
    "test cases for decode with code-map decode table"
    {:actual (u/decode decode-table-code-map [(ub 2r11101010)] 3)
     :expect (seq "cde")}
    {:actual (u/decode decode-table-code-map [(ub 2r00111101) (ub 2r11011010) (ub 2r0)] 6)
     :expect (seq "eekllu")}
    {:actual (u/decode decode-table-code-map [(ub 2r00111101) (ub 2r11011010) (ub 2r0)] 5)
     :expect (seq "eekll")}
    {:actual (u/decode decode-table-code-map [] 1)
     :expect '()}
    ;;
    ))

(deftest decode-tree-tests
  (testing-is-equal
    "test cases for decode with tree decode table"
    {:actual (u/decode-tree huffman-tree [(ub 2r11101010)] 3)
     :expect (seq "cde")}
    {:actual (u/decode-tree huffman-tree [(ub 2r00111101) (ub 2r11011010) (ub 2r0)] 6)
     :expect (seq "eekllu")}
    {:actual (u/decode-tree huffman-tree [(ub 2r00111101) (ub 2r11011010) (ub 2r0)] 5)
     :expect (seq "eekll")}
    {:actual (u/decode-tree huffman-tree [] 1)
     :expect '()}
    ;;
    ))
