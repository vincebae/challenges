(ns compression.huffman
  (:require
    [clojure.data.priority-map :refer [priority-map-keyfn]]))


(defn create-leaf-node
  "Create huffman tree leaf node"
  [value weight]
  {:value value :weight weight})


(defn merge-nodes
  "Merge two nodes into a new huffman tree"
  [lchild rchild]
  {:weight (+ (:weight lchild) (:weight rchild))
   :lchild lchild
   :rchild rchild})


(defn build-tree
  "Build a huffman tree from the input sequence"
  [input-seq]
  (letfn
    [(assoc-node
       ;; Add the node to the priority map with a random key
       [pmap node]
       (assoc pmap (keyword (gensym)) node))

     (build-tree-from-pmap
       ;; Helper function to build huffman tree from the priority map
       [pmap]
       (let [[[_ x] [_ y]] (take 2 pmap)]
         (if (nil? y)
           x
           (recur (assoc-node ((comp pop pop) pmap) (merge-nodes x y))))))]

    (->> input-seq
         frequencies
         (map #(apply create-leaf-node %))
         (reduce assoc-node (priority-map-keyfn :weight))
         build-tree-from-pmap)))
