(ns compression.code-util-test
  (:require
    [clojure.test :refer [deftest]]
    [compression.code-utils :as cu]
    [compression.test-utils :refer [testing-is-equal]]))


(def ub unchecked-byte)


(deftest bit-vector->byte-test
  (testing-is-equal
    "Test cases for bit-vector->byte."
    {:actual (cu/bit-vector->byte [])
     :expect (ub 0)}
    {:actual (cu/bit-vector->byte [0])
     :expect (ub 0)}
    {:actual (cu/bit-vector->byte [1])
     :expect (ub 0x80)}
    {:actual (cu/bit-vector->byte [1 0 1 1 0 1 0])
     :expect (ub 0xB4)}
    {:actual (cu/bit-vector->byte [1 0 1 1 0 1 0 0])
     :expect (ub 0xB4)}
    {:actual (cu/bit-vector->byte [1 0 1 1 0 1 0 0 1 0])
     :expect (ub 0xB4)}))


(deftest byte->bit-vector-test
  (testing-is-equal
    "Test cases for byte->bit-vector."
    {:actual (cu/byte->bit-vector (ub 0))
     :expect [0 0 0 0 0 0 0 0]}
    {:actual (cu/byte->bit-vector (ub 1))
     :expect [0 0 0 0 0 0 0 1]}
    {:actual (cu/byte->bit-vector (ub 0xB4))
     :expect [1 0 1 1 0 1 0 0]}))


(deftest bytes->bits-test
  (testing-is-equal
    "Test cases for bytes->bits."
    {:actual (cu/bytes->bits [])
     :expect []}
    {:actual (cu/bytes->bits [(ub 0)])
     :expect [0 0 0 0 0 0 0 0]}
    {:actual (cu/bytes->bits [(ub 0) (ub 1) (ub 0xB4)])
     :expect [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 0 1 0 0]}))


(deftest byte->code-test
  (testing-is-equal
    "Test cases for byte->code."
    {:actual (cu/byte->code (ub 0))
     :expect 0}
    {:actual (cu/byte->code (ub 1))
     :expect 1}
    {:actual (cu/byte->code (ub 0x80))
     :expect 128}
    {:actual (cu/byte->code (ub 0xFF))
     :expect 255}))


(deftest code->bytes-test
  (testing-is-equal
    "Test cases for code->bytes."
    {:actual (cu/code->bytes 0 1 true)
     :expect []}
    {:actual (cu/code->bytes 0 1 false)
     :expect [(ub 0)]}
    {:actual (cu/code->bytes 1 1 true)
     :expect []}
    {:actual (cu/code->bytes 1 1 false)
     :expect [(ub 0x80)]}
    {:actual (cu/code->bytes 0xABCDEF12345 44 true)
     :expect [(ub 0xAB) (ub 0xCD) (ub 0xEF) (ub 0x12) (ub 0x34)]}
    {:actual (cu/code->bytes 0xABCDEF12345 44 false)
     :expect [(ub 0xAB) (ub 0xCD) (ub 0xEF) (ub 0x12) (ub 0x34) (ub 0x50)]}))


(deftest keep-trailing-bits-test
  (testing-is-equal
    "Test cases for keep-trailing-bits-test"
    {:actual (cu/keep-trailing-bits 2r10011100 5)
     :expect 2r011100}
    {:actual (cu/keep-trailing-bits 0xABCDEF 12)
     :expect 0xDEF}))


(deftest append-code-test
  (testing-is-equal
    "Test cases for append-code."
    {:actual (cu/append-code 0x1234 0 0)
     :expect 0x1234}
    {:actual (cu/append-code 0x1234 0 4)
     :expect 0x12340}
    {:actual (cu/append-code 0x1234 2r0111 4)
     :expect 0x12347}))


(deftest append-byte-test
  (testing-is-equal
    "Test cases for append-byte."
    {:actual (cu/append-byte 0x1234 (ub 0))
     :expect 0x123400}
    {:actual (cu/append-byte 0x1234 (ub 0x56))
     :expect 0x123456}))
