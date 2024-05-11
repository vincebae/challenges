(ns compression.compressor
  (:require
    [compression.code-utils :as cu :refer [BITS-PER-BYTE]]))


(defn- encode-bit-vector
  "Encode the data into byte sequence using the encode table with codes as bit vectors."
  [encode-table data]
  (letfn
    [(seq->bits
       ;; Generate a lazy sequence of bits from the data
       ;; The next bit sequence can be easily looked up from the encode-table directly
       [data]
       (lazy-seq
         (when-let [[x & xs] (seq data)]
           (concat (encode-table x) (seq->bits xs)))))

     (encode-helper
       [bits]
       ;; Generate a lazy sequence of bytes from the bit sequence
       ;; by taking 8 bits from the bit sequence and convert them to a byte.
       (lazy-seq
         (when (not-empty bits)
           (cons (cu/bit-vector->byte bits)
                 (encode-helper (drop BITS-PER-BYTE bits))))))]

    (encode-helper (seq->bits data))))


(defn- encode-code-map
  "Encode the data into byte sequence using the encode table with codes as code maps."
  [encode-table data]
  (letfn
    [(encode-helper
       [data curr length]
       (lazy-seq
         (if-let [[x & xs] (seq data)]
           (let [code-data (encode-table x)
                 concatenated (cu/append-code curr (:code code-data) (:length code-data))
                 concatenated-length (+ length (:length code-data))
                 new-length (mod concatenated-length BITS-PER-BYTE)
                 new-curr (cu/keep-trailing-bits concatenated new-length)]
             (concat (cu/code->bytes concatenated concatenated-length #_?full-bytes-only= true)
                     (encode-helper xs new-curr new-length)))
           (cu/code->bytes curr length #_?full-bytes-only= false))))]

    (encode-helper data 0 0)))


(defn encode
  "Encode the data into byte sequence using the encode table."
  [encode-table data]
  (case (:code-type encode-table)
    :bit-vector (encode-bit-vector encode-table data)
    :code-map (encode-code-map encode-table data)
    nil))
