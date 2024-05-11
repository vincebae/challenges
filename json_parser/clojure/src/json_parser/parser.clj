;; https://codingchallenges.fyi/challenges/challenge-json-parser/

(ns json-parser.parser
  (:require
    [clojure.set :refer [union]]
    [json-parser.lexer :refer [tokenize]]
    [json-parser.utils :refer [error]]))


(declare build-map
         start-state
         read-pair-start-state
         read-pair-end-state
         start-list-state
         read-list-value-state)


(defn parse
  "Parse json text and build clojure map"
  [json-text]
  (let [res
        (->> json-text
             tokenize
             build-map)]
    (cond
      (= :error (first res)) res
      (empty? (second res)) (first res)
      :else (error "invalid token: " (first (second res))))))


(defn- build-map
  "Build clojure map from tokens from json string"
  [tokens]
  ;; starts the state machine recursively using trampoline.
  (trampoline start-state nil (seq tokens)))


(defn- build-list
  "Build clojure vector from tokens from string for json list"
  [tokens]
  ;; starts the state machine recursively using trampoline.
  (trampoline start-list-state [] (seq tokens)))


;; Constants
(def paired-data-tokens #{:string :number})
(def single-data-tokens #{:true :false :null :undefined})
(def data-tokens (union paired-data-tokens single-data-tokens))


;; Helper functions
(defn- error? [token] (= (first token) :error))
(defn- colon? [token] (= (first token) :colon))
(defn- open-curly? [token] (= (first token) :open-curly))
(defn- open-bracket? [token] (= (first token) :open-bracket))

(defn- get-token-data
  [token]
  (condp contains? (first token)
    paired-data-tokens (second token)
    single-data-tokens (first token)
    nil))


(defn- get-value
  "Get the first value parsed from the tokens.
   If parsed correctly, returns a vector with the parsed value and rest tokens.
   If not, returns an error token which is [:error error_message]"
  [[token & rest-tokens :as tokens]]
    (cond
      (open-curly? token) (build-map tokens)
      (open-bracket? token ) (build-list tokens)
      (data-tokens (first token)) (vector (get-token-data token) rest-tokens)
      :else (error "invalid token: " token " in read-pair-start-state")))


(defn- assoc-pair-to-map
  [curr key-token [first-token & rest-tokens]]
  (if (colon? first-token )
    (let
      [res (get-value rest-tokens)]
      (if (error? res)
        res
        (read-pair-end-state (assoc curr (keyword (second key-token)) (first res)) (second res))))
    (error "invalid token: " first-token " in read-pair-start-state")))


(defn- add-value-to-vector
  [curr tokens]
  (let
    [[x y :as res] (get-value tokens)]
    (if (error? res)
      res
      ;; check the next token after value has been read before continue
      (case (first (first y))
        nil (error "parse eneded in invalid state: read-list-value-state")
        :close-bracket (vector (conj curr x) (rest y))
        :comma (read-list-value-state (conj curr x) (rest y))
        :else (error "invalid token: " y "in start-list-state")))))


;; State machine functions
(defn- start-state
  [curr [token & rest-tokens]]
  (case (first token)
    nil nil
    :error token
    :open-curly (read-pair-start-state curr rest-tokens)
    (error "invalid token: " token, " in start-state")))


(defn- read-pair-start-state
  [curr [token & rest-tokens]]
  (case (first token)
    nil (error "parse ended in invalid state: read-pair-start-state")
    :error token
    :close-curly (vector (or curr {}) rest-tokens)
    :string (assoc-pair-to-map curr token rest-tokens)
    (error "invalid token: " token " in read-pair-start-state")))


(defn- read-pair-end-state
  [curr [token & rest-tokens]]
  (case (first token)
    nil (error "parse ended in invalid state: read-pair-end-state")
    :error token
    :comma (if (= (first (first rest-tokens)) :close-curly)
             (error "invalid token: " (first rest-tokens)
                    " after " token " in read-pair-end-state")
             (read-pair-start-state curr rest-tokens))
    :close-curly (vector curr rest-tokens)
    (error "invalid token: " token " in read-pair-end-state")))


(defn- start-list-state
  [curr [token & rest-tokens]]
  (case (first token)
    nil nil
    :error token
    :open-bracket (read-list-value-state curr rest-tokens)
    (error "invalid token: " token "in start-list-state")))


(defn- read-list-value-state
  [curr [token & rest-tokens :as tokens]]
  (case (first token)
    nil (error "parse eneded in invalid state: read-list-value-state")
    :error token
    :close-bracket (vector curr rest-tokens)
    (add-value-to-vector curr tokens)))
