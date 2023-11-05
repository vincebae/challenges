;; https://codingchallenges.fyi/challenges/challenge-json-parser/

(ns json-parser.utils)

(defn error
  "Create vector to indicate error formatted in [:error \"error messages\"]"
  [& messages]
  (vector :error (apply str messages)))

