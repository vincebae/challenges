(ns compression.huffman-test
  (:require
    [clojure.test :refer [deftest]]
    [compression.huffman :as h]
    [compression.test-utils :refer [testing-is-equal freq->seq]]))


(deftest tree-node-test
  (testing-is-equal
    "test cases for tree node functions"
    {:actual (h/create-leaf-node \a 100)
     :expect {:value \a :weight 100}}
    {:actual (h/merge-nodes {:value \a :weight 100} {:value \b :weight 10})
     :expect {:weight 110 :lchild {:value \a :weight 100} :rchild {:value \b :weight 10}}}))


(deftest build-tree-test
  (testing-is-equal
    "test cases for building huffman tree"
    {:actual (h/build-tree "")
     :expect nil}
    {:actual (h/build-tree "a")
     :expect {:value \a :weight 1}}
    {:actual (h/build-tree "aba")
     :expect {:weight 3 :lchild {:value \b :weight 1} :rchild {:value \a :weight 2}}}
    {:actual (h/build-tree "aabbaca")
     :expect {:weight 7
              :lchild {:weight 3
                       :lchild {:value \c :weight 1}
                       :rchild {:value \b :weight 2}}
              :rchild {:value \a :weight 4}}}
    {:actual (h/build-tree (freq->seq {\c 32, \d 42, \e 120, \k 7, \l 43, \m 24, \u 37, \z 2}))
     :expect {:weight 307
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
                                                  :rchild {:value \m :weight 24}}}}}}}))
