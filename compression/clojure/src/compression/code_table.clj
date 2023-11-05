(ns compression.code-table)


(defn build-encode-table
  "Build a code table from the given Huffman tree."
  ([node] (build-encode-table node :code-map))
  ([node code-type]
   (letfn
     [(build-table-bit-vector
        ;; Codes are represented as a vector of bits
        [node bit-vector table]
        (cond
          (nil? node) table
          (:value node) (assoc table (:value node) (or (not-empty bit-vector) [0]))
          :else (->> table
                     (build-table-bit-vector (:lchild node) (conj bit-vector 0))
                     (build-table-bit-vector (:rchild node) (conj bit-vector 1)))))

      (build-table-code-map
        ;; Codes are represented as a map with :code and :length keyworkds
        [node depth code table]
        (cond
          (nil? node) table
          (:value node) (assoc table (:value node) {:length (max depth 1) :code code})
          :else (let
                  [shifted (bit-shift-left code 1)]
                  (->> table
                       (build-table-code-map (:lchild node) (inc depth) shifted)
                       (build-table-code-map (:rchild node) (inc depth) (bit-set shifted 0))))))]

     (case code-type
       :code-map (build-table-code-map node 0 0 {})
       :bit-vector (build-table-bit-vector node [] {})
       nil))))
