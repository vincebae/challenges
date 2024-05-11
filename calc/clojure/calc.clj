#!/usr/bin/env bb

(ns calc
  (:require
   [babashka.cli :refer [parse-args]]
   [clojure.core.match :refer [match]]
   [clojure.test :refer [run-tests deftest is testing]]))

(def ^:private cli-options
  {:alias {:h :help
           :t :test}
   :coerce {:help :boolean
            :test :boolean}})

(def ^:private DIGITS (into #{} "0123456789"))

(def ^:private SYMBOL-MAP
  {\+ :plus
   \- :minus
   \* :times
   \/ :divides
   \( :open
   \) :close})

(def ^:private OPERATOR-MAP
  {:plus {:fn + :precedence 1}
   :minus {:fn - :precedence 1}
   :times {:fn * :precedence 2}
   :divides {:fn / :precedence 2}})

;; Declare state-machine functions for tokenize
(declare normal-state
         number-state)

(defn- tokenize
  [^String line]
  (trampoline normal-state (seq line) []))

(defn- ch-seq->number
  [ch-seq]
  (let [s (apply str ch-seq)]
    (if (re-find #"^-?\d+.?\d*$" s)
      (read-string s)
      :invalid-token-error)))

(defn- normal-state
  [ch-seq acc]
  (let [[ch & ch-rest] ch-seq
        read-number (fn [] (number-state ch-rest [ch] acc))
        read-symbol (fn [] (normal-state ch-rest (conj acc (SYMBOL-MAP ch))))]
    (cond
      (nil? ch) acc
      (Character/isWhitespace ch) (normal-state ch-rest acc)
      (= ch \-) (if (number? (peek acc)) (read-symbol) (read-number))
      (DIGITS ch) (read-number)
      (SYMBOL-MAP ch) (read-symbol)
      :else [:invalid-token-error])))

(defn- number-state
  [ch-seq curr acc]
  (let [[ch & ch-rest] ch-seq]
    (cond
      (or (DIGITS ch) (= ch \.)) (number-state ch-rest (conj curr ch) acc)
      :else (normal-state ch-seq (conj acc (ch-seq->number curr))))))

(defn- infix->postfix
  ([tokens] (infix->postfix tokens [] ()))
  ([tokens acc stack]
   (match tokens
     ;; no more tokens. move all operators from stack to acc
     [] (into acc stack)
     ;; simply add open parenthesis to the stack
     [:open & r] (recur r acc (conj stack :open))
     ;; add all the operators in stack until hits open parenthesis.
     [:close & r]
     (let [[upper lower] (split-with (complement #{:open}) stack)]
       (if (empty? lower)
         (throw (IllegalArgumentException. "Invalid expression"))
         (recur r (into acc upper) (rest lower))))
     ;; simply put number to acc
     [(n :guard number?) & r] (recur r (conj acc n) stack)
     ;; check precedent and put the operator to stack.
     [(o :guard keyword?) & r]
     (let [top (peek stack)
           precedence-top (or (get-in OPERATOR-MAP [top :precedence]) 0)
           precedence-curr (get-in OPERATOR-MAP [o :precedence])]
       (if (>= precedence-top precedence-curr)
         (recur r (conj acc top) (conj (pop stack) o))
         (recur r acc (conj stack o))))
     :else (throw (IllegalArgumentException. "Invalid expression")))))

(defn- calc-postfix
  ([tokens] (calc-postfix (seq tokens) ()))
  ([tokens stack]
   (let [[x & xs] tokens]
     (cond
       (number? x) (recur xs (conj stack x))
       (and (nil? x) (= (count stack) 1)) (first stack)
       (and (keyword? x) (>= (count stack) 2))
       (let [[a b & r] stack
             operator (-> OPERATOR-MAP x :fn)]
         (recur xs (conj r (operator b a))))
       :else (throw (IllegalArgumentException.
                     (str "Invalid expression: " tokens ", " stack)))))))

(defn- calc-line
  [^String line]
  (->> line
       tokenize
       infix->postfix
       calc-postfix))

(defn- print-help
  [arg-map]
  (println "arg-map:")
  (println arg-map))

(defn- run
  [args _]
  (doseq [line args]
    (println (calc-line line))))

;; Test cases
(deftest calc-tests
  (testing "Test cases for calc-line no operators"
    (is (= (calc-line "1") 1))
    (is (= (calc-line "-1") -1))
    (is (= (calc-line "(1)") 1))
    (is (= (calc-line "(-1)") -1))
    (is (= (calc-line "1.234") 1.234))
    (is (= (calc-line "-1.234") -1.234))
    (is (= (calc-line "(1.234)") 1.234))
    (is (= (calc-line "(-1.234)") -1.234)))

  (testing "Test cases for calc-line with operators"
    (is (= (calc-line "1 + 2") 3))
    (is (= (calc-line "10 - 4") 6))
    (is (= (calc-line "2 * 4") 8))
    (is (= (calc-line "10 / 2") 5))
    (is (= (calc-line "2 + 3 * 4") 14))
    (is (= (calc-line "2 * 3 + 4") 10))
    (is (= (calc-line "2 * (3 + 4)") 14))
    (is (= (calc-line "(2 + 3) * 4") 20))
    (is (= (calc-line "2 * 3 + 4 * 5") 26))
    (is (= (calc-line "2 * 3 * 4 * 5") 120))
    (is (= (calc-line "2 * (3 + 4) * 5") 70)))

  (testing "Test cases for calc-line (no-whitespaces)"
    (is (= (calc-line "1+2") 3))
    (is (= (calc-line "10-4") 6))
    (is (= (calc-line "2*4") 8))
    (is (= (calc-line "10/2") 5))
    (is (= (calc-line "2+3*4") 14))
    (is (= (calc-line "2*3+4") 10))
    (is (= (calc-line "2*(3+4)") 14))
    (is (= (calc-line "(2+3)*4") 20))
    (is (= (calc-line "2*3+4*5") 26))
    (is (= (calc-line "2*3*4*5") 120))
    (is (= (calc-line "2*(3+4)*5") 70)))

  (testing "Test cases for tokenize"
    (is (= (tokenize "1 + 2") [1 :plus 2]))
    (is (= (tokenize "1 - 2") [1 :minus 2]))
    (is (= (tokenize "1 * 2") [1 :times 2]))
    (is (= (tokenize "1 / 2") [1 :divides 2]))
    (is (= (tokenize "1 + 2 * 3") [1 :plus 2 :times 3]))
    (is (= (tokenize "(1 + 2) * 3") [:open 1 :plus 2 :close :times 3])))

  (testing "Test cases for infix->postfix"
    (is (= (infix->postfix [1 :plus 2]) [1 2 :plus]))
    (is (= (infix->postfix [1 :minus 2]) [1 2 :minus]))
    (is (= (infix->postfix [1 :times 2]) [1 2 :times]))
    (is (= (infix->postfix [1 :divides 2]) [1 2 :divides]))
    (is (= (infix->postfix [1 :plus 2 :times 3]) [1 2 3 :times :plus]))
    (is (= (infix->postfix [1 :times 2 :plus 3]) [1 2 :times 3 :plus]))
    (is (= (infix->postfix [:open 1 :plus 2 :close :times 3]) [1 2 :plus 3 :times]))
    (is (= (infix->postfix [1 :times :open 2 :plus 3 :close :times 4])
           [1 2 3 :plus :times 4 :times])))

  (testing "Test cases for calc-postfix"
    (is (= (calc-postfix [1 2 :plus]) 3)) ; 1 + 2
    (is (= (calc-postfix [10 4 :minus]) 6)) ; 10 - 4
    (is (= (calc-postfix [2 4 :times]) 8)) ; 2 * 4
    (is (= (calc-postfix [10 2 :divides]) 5)) ; 10 / 2
    (is (= (calc-postfix [1 2 :plus 3 :times]) 9)) ; (1 + 2) * 3
    (is (= (calc-postfix [1 2 3 :plus :times]) 5)) ; 1 * (2 + 3)
    (is (= (calc-postfix [1 2 3 :times :plus]) 7)) ; 1 + 2 * 3
    ;; (1 + 2 * 3 - 4 * 5 + 25) / 3
    (is (= (calc-postfix [1 2 3 :times :plus 4 5 :times :minus 25 :plus 3 :divides]) 4))))

(defn -main
  [& args]
  (try
    (let [{:keys [args opts] :as arg-map} (parse-args args cli-options)]
      (cond
        (:test opts) (run-tests 'calc)
        (:help opts) (print-help arg-map)
        :else (run args opts)))
    (catch Exception e
      (println "Error:" (.getMessage e)))))

(apply -main *command-line-args*)
