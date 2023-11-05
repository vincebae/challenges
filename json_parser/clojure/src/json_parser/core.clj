;; https://codingchallenges.fyi/challenges/challenge-json-parser/

(ns json-parser.core
  (:require
   [json-parser.parser :refer [parse]]))

(defn json->edn
  "Convert json file to clojure EDN format"
  [filename]
  (->> (slurp filename)
       (parse)))

(defn -main
  [args]
  (if (empty? args)
    (prn (json->edn *in*))
    (doseq [arg args]
      (prn (json->edn arg)))))
