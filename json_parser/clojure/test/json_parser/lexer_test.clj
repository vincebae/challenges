(ns json-parser.lexer-test
  (:require
   [clojure.test :refer [deftest is]]
   [json-parser.lexer :refer [tokenize]]
   [json-parser.test-utils :refer [expand-test-cases testing-is-equal]]))

(defmacro testing-tokenize-error
  [desc & cases]
  (list `expand-test-cases desc cases
        (fn [{input# :input error# :error}]
          `(is (and (= (first (last ~input#)) :error)
                    (= (second (last ~input#)) ~error#))))))

(deftest tokenize-whitespaces-only
  (testing-is-equal
   "tokenize test cases for whitespaces only"
   {:actual (tokenize "")
    :expect nil}
   {:actual (tokenize " ")
    :expect nil}
   {:actual (tokenize "\n")
    :expect nil}))

(deftest tokenize-special-characters
  (testing-is-equal
   "tokenize test cases for special characters"
   {:actual (tokenize "{ } [ ] : ,")
    :expect [[:open-curly] [:close-curly]
             [:open-bracket] [:close-bracket]
             [:colon] [:comma]]}))

(deftest tokenize-strings
  (testing-is-equal
   "tokenize test cases for strings"
   {:desc "string in double quotes"
    :actual (tokenize "\"key\"")
    :expect [[:string "key"]]}
   {:desc "string in single quotes"
    :actual (tokenize "'key'")
    :expect [[:string "key"]]}))

(deftest tokenize-invalid-strings
  (testing-tokenize-error
   "tokenize error test cases for strings"
   {:desc "unmatched quote #1"
    :input (tokenize "\"abc'")
    :error "tokenize ended in invalid state: str-double-state"}
   {:desc "unmatched quote #2"
    :input (tokenize "'abc\"")
    :error "tokenize ended in invalid state: str-single-state"}))

(deftest tokenize-numbers
  (testing-is-equal
   "tokenize test cases for numbers"
   {:desc "integer number"
    :actual (tokenize "123")
    :expect [[:number 123]]}
   {:desc "integer number with leading plus sign"
    :actual (tokenize "+123")
    :expect [[:number 123]]}
   {:desc "negative integer number"
    :actual (tokenize "-123")
    :expect [[:number -123]]}
   {:desc "float number"
    :actual (tokenize "1.23")
    :expect [[:number 1.23]]}
   {:desc "float number with leading plus sign"
    :actual (tokenize "+1.23")
    :expect [[:number 1.23]]}
   {:desc "negative float number"
    :actual (tokenize "-1.23")
    :expect [[:number -1.23]]}))

(deftest tokenize-invalid-numbers
  (testing-tokenize-error
   "tokenize error test cases for numbers"
   {:desc "no number after plus sign"
    :input (tokenize "+")
    :error "invalid number: +"}
   {:desc "no number after minus sign"
    :input (tokenize "-")
    :error "invalid number: -"}
   {:desc "multiple dot in number"
    :input (tokenize "1.2.3")
    :error "invalid number: 1.2.3"}))

(deftest tokenize-symbols
  (testing-is-equal
   "tokenize test cases for symbols"
   {:actual (tokenize "true")
    :expect [[:true]]}
   {:actual (tokenize "false")
    :expect [[:false]]}
   {:actual (tokenize "null")
    :expect [[:null]]}
   {:actual (tokenize "undefined")
    :expect [[:undefined]]}))

(deftest tokenize-invalid-symbols
  (testing-tokenize-error
   "tokenize error test cases for invalid symbols"
   {:desc "invalid symbol: abc"
    :input (tokenize "abc")
    :error "invalid symbols: abc"}))

(deftest tokenize-combined
  (testing-is-equal
   "tokenize test cases with combined tokens"
   {:actual (tokenize
             (str "{\n"
                  "  \"key1\": 'value',\n"
                  "  'key2': 123,\n"
                  "  'key3': -1.234,\n"
                  "  'key4': null,\n"
                  "  'key5': undefined,\n"
                  "  'key6': true,\n"
                  "  'key7': false\n"
                  "}"))
    :expect [[:open-curly]
             [:string "key1"] [:colon] [:string "value"] [:comma]
             [:string "key2"] [:colon] [:number 123] [:comma]
             [:string "key3"] [:colon] [:number -1.234] [:comma]
             [:string "key4"] [:colon] [:null] [:comma]
             [:string "key5"] [:colon] [:undefined] [:comma]
             [:string "key6"] [:colon] [:true] [:comma]
             [:string "key7"] [:colon] [:false]
             [:close-curly]]}))
