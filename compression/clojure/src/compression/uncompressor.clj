(ns compression.uncompressor
  (:require
    [compression.code-utils :as cu :refer [BITS-PER-BYTE]]))


(defn- decode-bit-vector
  "Decode the byte sequence back to a data sequence
   using the decode table with codes as bit vectors and the final length of the data sequence."
  [decode-table bytes length]
  (letfn
    [(decode-bit-vector-helper
       [bits length]
       (lazy-seq
         (when (pos? length)
           (loop [[x & xs] (seq bits) curr []]
             (when x
               (let [new-curr (conj curr x)
                     ch (decode-table new-curr)]
                 (if ch
                   (cons ch (decode-bit-vector-helper xs (dec length)))
                   (recur xs new-curr))))))))]

    (decode-bit-vector-helper (cu/bytes->bits bytes) length)))


(defn- decode-code-map
  "Decode the byte sequence back to a data sequence
   using the decode table with codes as code maps and the final length of the data sequence."
  [decode-table bytes length]
  (letfn
    [(decode-code-map-helper
       [bytes length loaded-code loaded-code-length]
       (lazy-seq
         (when (pos? length)
           (loop
             [bytes bytes
              loaded-code loaded-code
              loaded-code-length loaded-code-length
              curr-code-length 1]
             (if (<= curr-code-length loaded-code-length)
               ;; try to find the code from the code map without loading more byte.
               (let [extra-code-length (- loaded-code-length curr-code-length)
                     curr-code (bit-shift-right loaded-code extra-code-length),
                     ch (get-in decode-table [curr-code-length curr-code])]
                 (if ch
                   ;; Code found, so continue the lazy sequence with the found data.
                   (let [new-loaded-code (cu/keep-trailing-bits loaded-code extra-code-length)]
                     (cons ch
                           (decode-code-map-helper bytes
                                                   (dec length)
                                                   new-loaded-code
                                                   extra-code-length)))
                   ;; Code not found, so try with the increased code length.
                   (recur bytes loaded-code loaded-code-length (inc curr-code-length))))
               ;; Loaded code is not enough to proceed, so load next byte and continue.
               (when-let [[x & xs] (seq bytes)]
                 (let [new-loaded-code (cu/append-byte loaded-code x)
                       new-loaded-code-length (+ loaded-code-length BITS-PER-BYTE)]
                   (recur xs new-loaded-code new-loaded-code-length curr-code-length))))))))]

    (decode-code-map-helper bytes length 0 0)))


(defn decode-tree
  "Decode the byte sequence back to a data sequence
   using the huffman tree and the final length of the data sequence."
  [tree bytes length]
  (letfn
    [(decode-tree-helper
       [bits length]
       (lazy-seq
         (when (pos? length)
           (loop [[x & xs] (seq bits) curr tree]
             (when x
               (let [new-curr (if (zero? x) (:lchild curr) (:rchild curr))
                     value (:value new-curr)]
                 (if value
                   (cons value (decode-tree-helper xs (dec length)))
                   (recur xs new-curr))))))))]

    (decode-tree-helper (cu/bytes->bits bytes) length)))


(defn decode
  "Decode the byte seqnece back to a data sequence
   using the decode table and the final length of the data sequence."
  [decode-table bytes length]
  (case (:code-type decode-table)
    :bit-vector (decode-bit-vector decode-table bytes length)
    :code-map (decode-code-map decode-table bytes length)
    nil))
