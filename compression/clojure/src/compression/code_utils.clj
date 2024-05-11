;; Collection of utility functions that deals with code, byte and bit operations.
;; Note that
;; - code is long type
;; - byte is byte type
;; - bit is just either 0 or 1
(ns compression.code-utils)

(def BITS-PER-BYTE 8)


(def ALL-ONE
  "long value with all the bits set to 1"
  (bit-not 0))


(defn bit-vector->byte
  "Take the first 8 bits from the bit vector and convert to a byte.
   If the bit-vector contains less than 8 bits, 0's are padded to the back of the vector."
  [bit-vector]
  (->> (concat bit-vector (repeat 0))
       (take BITS-PER-BYTE)
       (reduce #(bit-or (bit-shift-left %1 1) %2) 0)
       unchecked-byte))


(def byte->bit-vector
  "Convert a byte to a bit vector."
  (memoize
    (fn [byte]
      (->> (range BITS-PER-BYTE)
           reverse
           (map #(bit-test byte %))
           (map {false 0 true 1})
           vec))))


(defn bytes->bits
  "Generate a lazy sequence of bits from a sequence of bytes."
  [bytes]
  (lazy-seq
    (when-let [[x & xs] (seq bytes)]
      (concat (byte->bit-vector x) (bytes->bits xs)))))


(defn byte->code
  "Convert a (signed) byte to a code which is a long type."
  [byte]
  (bit-and 0xFF byte))


(defn code->bytes
  "Convert a code with the given length to a sequence of bytes.
   If ?full-byte-only is true, only full bytes are converted and rest are ignored.
   For example, for a code with length of 18, it will generate a sequence of two bytes.
   If ?full-byte-only is false, the final byte is padded with 0 until it has 8 bits"
  [code length ?full-bytes-only]
  (if (zero? length)
    []
    (let [modded (mod length BITS-PER-BYTE)
          shift-counts (reverse (range modded length BITS-PER-BYTE))
          full-bytes
          (->> (map #(bit-shift-left 0xFF %) shift-counts)
               (map #(bit-and code %))
               (map #(bit-shift-right %2 %1) shift-counts)
               (map unchecked-byte))]
      (if (or ?full-bytes-only (zero? modded))
        full-bytes
        (conj (vec full-bytes)
              (->> (bit-shift-left code (- BITS-PER-BYTE modded))
                   (bit-and 0xFF)
                   (unchecked-byte)))))))


(defn keep-trailing-bits
  "Keep the trailing bits of the given length from the code and set leading bits to 0."
  [code length]
  (-> ALL-ONE
      (bit-shift-left length)
      (bit-and code)
      (bit-xor code)))


(defn append-code
  "Append the bits from the code 2 to the end of the code 1."
  [code1 code2 code2-length]
  (bit-or (bit-shift-left code1 code2-length) code2))


(defn append-byte
  "Append the bits from the byte to the end of the code."
  [code byte]
  (append-code code (byte->code byte) BITS-PER-BYTE))
