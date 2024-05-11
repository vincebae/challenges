;; https://codingchallenges.fyi/challenges/challenge-json-parser/

(ns json-parser.lexer
  [:require [clojure.string :as s]
   [json-parser.utils :refer [error]]])

;; Declare state-machine functions
(declare normal-state
         str-double-state
         str-single-state
         number-state
         symbol-state)

(defn tokenize
  "Tokenize given string into token vector using state machine."
  [input-string]
    ;; starts the state machine recursively using trampoline.
  (trampoline normal-state (lazy-seq input-string)))

(defmacro lazy-tokenize
  "Convenience macro to create lazy sequence for tokenization"
  [token ch-rest]
  `(lazy-seq (cons ~token (tokenize ~ch-rest))))

;; Constants used for tokenization
(def number-char-set (into #{} ".0123456789"))
(def all-number-char-set (into number-char-set "+-"))
(def symbols #{:true :false :null :undefined})

;; Helper functions
(defn- str->number
  [data]
  (try
    (let [parsed (read-string data)]
      (if (number? parsed) parsed nil))
    (catch Exception _ nil)))

(defn- starts-with?
  [ch-seq string]
  (= (take (count string) ch-seq) (seq string)))

;; State machine functions
(defn- normal-state
  [[ch & ch-rest :as ch-seq]]
  (cond
    (nil? ch) nil
    (all-number-char-set ch) (number-state (vector ch) ch-rest)
    (s/blank? (str ch)) (normal-state ch-rest)
    (= ch \{) (lazy-tokenize [:open-curly] ch-rest)
    (= ch \}) (lazy-tokenize [:close-curly] ch-rest)
    (= ch \[) (lazy-tokenize [:open-bracket] ch-rest)
    (= ch \]) (lazy-tokenize [:close-bracket] ch-rest)
    (= ch \:) (lazy-tokenize [:colon] ch-rest)
    (= ch \,) (lazy-tokenize [:comma] ch-rest)
    (= ch \") (str-double-state [] ch-rest)
    (= ch \') (str-single-state [] ch-rest)
    :else (symbol-state ch-seq)))

(defn- str-double-state
  [curr [ch & ch-rest]]
  (cond
    (nil? ch) (vector (error "tokenize ended in invalid state: str-double-state"))
    (= ch \") (lazy-tokenize [:string (apply str curr)] ch-rest)
    :else (str-double-state (conj curr ch) ch-rest)))

(defn- str-single-state
  [curr [ch & ch-rest]]
  (cond
    (nil? ch) (vector (error "tokenize ended in invalid state: str-single-state"))
    (= ch \') (lazy-tokenize [:string (apply str curr)] ch-rest)
    :else (str-single-state (conj curr ch) ch-rest)))

(defn- number-state
  [curr [ch & ch-rest :as ch-seq]]
  (cond
    (number-char-set ch) (number-state (conj curr ch) ch-rest)
    :else (if-let [number (str->number (apply str curr))]
            (lazy-tokenize [:number number] ch-seq)
            (vector (error "invalid number: " (apply str curr))))))

(defn- symbol-state
  [ch-seq]
  (if-let [matched (some #(when (starts-with? ch-seq (name %)) %) symbols)]
    (as-> (name matched) res
      (count res)
      (drop res ch-seq)
      (lazy-tokenize (vector matched) res))
    (vector (error "invalid symbols: " (apply str (take 9 ch-seq))))))


