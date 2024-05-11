(ns compression.compressor-test
  (:require
    [clojure.test :refer [deftest]]
    [compression.compressor :as c]
    [compression.test-utils :refer [testing-is-equal]]))


(def ub unchecked-byte)


(def code-table-bit-vector
  {:code-type :bit-vector
   \c [1 1 1 0]
   \d [1 0 1]
   \e [0]
   \k [1 1 1 1 0 1]
   \l [1 1 0]
   \m [1 1 1 1 1]
   \u [1 0 0]
   \z [1 1 1 1 0 0]})


(def code-table-code-map
  {:code-type :code-map
   \c {:length 4 :code 2r1110}
   \d {:length 3 :code 2r101}
   \e {:length 1 :code 0}
   \k {:length 6 :code 2r111101}
   \l {:length 3 :code 2r110}
   \m {:length 5 :code 2r11111}
   \u {:length 3 :code 2r100}
   \z {:length 6 :code 2r111100}})


(deftest ch-seq->bytes-bit-vector-test
  (testing-is-equal
    "test cases for seq->bytes for code table with bit-vector"
    {:actual (c/encode code-table-bit-vector "")
     :expect []}
    {:actual (c/encode code-table-bit-vector "cde")
     :expect [(ub 0xEA)]}
    {:actual (c/encode code-table-bit-vector "eekllul")
     :expect [(ub 0x3D) (ub 0xDA) (ub 0x60)]}))


(deftest ch-seq->bytes-code-map-test
  (testing-is-equal
    "test cases for seq->bytes for code table with code-map"
    {:actual (c/encode code-table-code-map "")
     :expect []}
    {:actual (c/encode code-table-code-map "cde")
     :expect [(ub 0xEA)]}
    {:actual (c/encode code-table-code-map "eekllul")
     :expect [(ub 0x3D) (ub 0xDA) (ub 0x60)]}))


;; (deftest bits->byte-tests
;;   (testing-is-equal
;;    "test cases for bits->byte"
;;    {:actual (c/bits->byte [])
;;     :expect (ub 0)}
;;    {:actual (c/bits->byte [0])
;;     :expect (ub 0)}
;;    {:actual (c/bits->byte [1])
;;     :expect (ub 0x80)}
;;    {:actual (c/bits->byte [1 1 0 0 0 0 1 1])
;;     :expect (ub 0xC3)}
;;    {:actual (c/bits->byte [1 1 0 0 0 1])
;;     :expect (ub 0xC4)}
;;    {:actual (c/bits->byte [0 1 1 0 0 0 0 1 1 0])
;;     :expect (ub 0x61)}))

;; (deftest code->bits-tests
;;   (testing-is-equal
;;    "test cases for code->bits"
;;    {:actual (c/code->bits 0)
;;     :expect [0 0 0 0 0 0 0 0]}
;;    {:actual (c/code->bits 0x80)
;;     :expect [1 0 0 0 0 0 0 0]}
;;    {:actual (c/code->bits 0xC3)
;;     :expect [1 1 0 0 0 0 1 1]}
;;    {:actual (c/code->bits 0xC4)
;;     :expect [1 1 0 0 0 1 0 0]}
;;    {:actual (c/code->bits 0x61)
;;     :expect [0 1 1 0 0 0 0 1]}
;;    {:actual (c/code->bits 0 1)
;;     :expect [0]}
;;    {:actual (c/code->bits 1 1)
;;     :expect [1]}
;;    {:actual (c/code->bits 2r00 2)
;;     :expect [0 0]}
;;    {:actual (c/code->bits 2r10 2)
;;     :expect [1 0]}
;;    {:actual (c/code->bits 0x40C3 15)
;;     :expect [1 0 0 0 0 0 0 1 1 0 0 0 0 1 1]}
;;     ;;
;;    ))
;; (deftest string->bits-tests
;;   (testing-is-equal
;;    "test cases for string->bits"
;;    {:actual (c/ch-seq->bits code-table "")
;;     :expect []}
;;    {:actual (c/ch-seq->bits code-table "cde")
;;     :expect [1 1 1 0 1 0 1 0]}
;;    {:actual (c/ch-seq->bits code-table "eekllul")
;;     :expect [0 0 1 1 1 1 0 1 1 1 0 1 1 0 1 0 0 1 1 0]}
;;     ;;
;;    ))
