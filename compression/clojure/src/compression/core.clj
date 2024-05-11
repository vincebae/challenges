;; https://codingchallenges.fyi/challenges/challenge-huffman

(ns compression.core
  (:gen-class)
  (:require
    [compression.compressor :as c]
    [compression.uncompressor :as u]
    [criterium.core :refer [bench]]))


;; (def testfile "test/huffman/testfiles/135-0.txt")
;;
;; (def freq-map
;;   (->> testfile
;;        slurp
;;        frequencies))
;;
;; (defn print-count
;;   [ch]
;;   (println "Count of" ch "is" (freq-map ch 0)))

(def encode-table-bit-vector
  {:code-type :bit-vector
   \c [1 1 1 0]
   \d [1 0 1]
   \e [0]
   \k [1 1 1 1 0 1]
   \l [1 1 0]
   \m [1 1 1 1 1]
   \u [1 0 0]
   \z [1 1 1 1 0 0]})


(def encode-table-code-map
  {:code-type :code-map
   \c {:length 4 :code 2r1110}
   \d {:length 3 :code 2r101}
   \e {:length 1 :code 0}
   \k {:length 6 :code 2r111101}
   \l {:length 3 :code 2r110}
   \m {:length 5 :code 2r11111}
   \u {:length 3 :code 2r100}
   \z {:length 6 :code 2r111100}})


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


(defn freq->str
  [freq-map]
  (->> freq-map
       (map #(repeat (second %) (first %)))
       (reduce into [])
       (apply str)))


(def input (freq->str {\c 32, \d 42, \e 120, \k 7, \l 43, \m 24, \u 37, \z 2}))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (if (=
        (doall (c/encode encode-table-bit-vector input))
        (doall (c/encode encode-table-code-map input)))
    (println "encode same")
    (println "encode different"))
  (let [encoded (c/encode encode-table-code-map input)
        length (count input)]
    (if (=
          (seq input)
          (doall (u/decode decode-table-bit-vector encoded length))
          (doall (u/decode decode-table-code-map encoded length))
          (doall (u/decode-tree huffman-tree encoded length)))
      (println "decode same")
      (do
        (println "decode different")
        (println "input")
        (println (seq input))
        (println "decoded bit-vector")
        (println (u/decode decode-table-bit-vector encoded length))
        (println "decoded code-map")
        (println (u/decode decode-table-code-map encoded length))
        (println "decoded tree")
        (println (u/decode-tree huffman-tree encoded length))))

    (println "Bench decode with bit-vector")
    (bench (doall (u/decode decode-table-bit-vector encoded length)))
    (println "Bench decode with code-and-length")
    (bench (doall (u/decode decode-table-code-map encoded length)))
    (println "Bench decode with huffman tree")
    (bench (doall (u/decode-tree huffman-tree encoded length)))))


;; (println "Bench encode with bit-vector")
;; (bench (doall (c/seq->bytes encode-table-bit-vector input)))
;; (println "Bench encode with code-and-length")
;; (bench (doall (c/seq->bytes encode-table-code-map input))))
