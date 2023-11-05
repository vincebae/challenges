(ns json-parser.parser-test
  (:require
    [clojure.test :refer [deftest is]]
    [json-parser.parser :refer [parse]]
    [json-parser.test-utils :refer [expand-test-cases testing-is-equal]]))


(defmacro testing-parse-error
  [desc & cases]
  (list `expand-test-cases desc cases
        (fn [{input# :input error# :error}]
          `(is (and (= (first ~input#) :error)
                    (= (second ~input#) ~error#))))))


(deftest parse-simple-form
  (testing-is-equal
    "parse test cases for very basic json"
    {:actual (parse "{}")
     :expect {}}
    {:actual (parse "{ }")
     :expect {}}
    {:actual (parse "")
     :expect nil}
    {:actual (parse "\n")
     :expect nil}))


(deftest parse-tests-simple-error-cases
  (testing-parse-error
    "parse error test cases for simple malformatting"
    {:desc "curly bracket unclosed"
     :input (parse "{")
     :error "parse ended in invalid state: read-pair-start-state"}
    {:desc "curly bracket closed without open"
     :input (parse "}")
     :error "invalid token: [:close-curly] in start-state"}
    {:desc "curly bracket opened twice"
     :input (parse "{{")
     :error "invalid token: [:open-curly] in read-pair-start-state"}))


(deftest parse-basic-key-value-pairs
  (testing-is-equal
    "parse test cases for basic key value pairs with various types"
    {:desc "key and string value pair both with double quotes"
     :actual (parse "{\"key\": \"value\"}")
     :expect {:key "value"}}
    {:desc "key and string value pair both with single quotes"
     :actual (parse "{'key': 'value'}")
     :expect {:key "value"}}
    {:desc "whole number value"
     :actual (parse "{'key': 123}")
     :expect {:key 123}}
    {:desc "float number value"
     :actual (parse "{'key': -123.456}")
     :expect {:key -123.456}}
    {:desc "boolean true value"
     :actual (parse "{'key': true}")
     :expect {:key :true}}
    {:desc "boolean false value"
     :actual (parse "{'key': false}")
     :expect {:key :false}}
    {:desc "null value"
     :actual (parse "{'key': null}")
     :expect {:key :null}}
    {:desc "undefined value"
     :actual (parse "{'key': undefined}")
     :expect {:key :undefined}}
    {:desc "multiple key and value pairs delimited by comma"
     :actual (parse
               (str "{\n"
                    "  \"key1\": 'value',\n"
                    "  'key2': 123,\n"
                    "  'key3': -1.234,\n"
                    "  'key4': null,\n"
                    "  'key5': undefined,\n"
                    "  'key6': true,\n"
                    "  'key7': false\n"
                    "}"))
     :expect {:key1 "value",
              :key2 123,
              :key3 -1.234,
              :key4 :null,
              :key5 :undefined,
              :key6 :true,
              :key7 :false}}))


(deftest parse-tests-nested-objects
  (testing-is-equal
    "parse test cases for nested objects"
    {:desc "2-level simple nested object"
     :actual (parse "{'outer-key' : {'inner-key' : 'value'}}")
     :expect {:outer-key {:inner-key "value"}}}
    {:desc "3-level simple nested object"
     :actual (parse "{'outer-key' : {'middle-key' : {'inner-key' : 'value'}}}")
     :expect {:outer-key {:middle-key {:inner-key "value"}}}}
    {:desc "complex nested object"
     :actual (parse
               (str "{\n"
                    " 'key1' : 'value', \n"
                    " 'key2' : { 'key3' : 123, 'key4' : -1.234 },\n"
                    " 'key5' : { 'key6' : true, 'key7' : false},\n"
                    " 'key8' : { 'key9' : null, 'key10' : { 'key11' : undefined }}\n"
                    "}"))
     :expect {:key1 "value",
              :key2 {:key3 123, :key4 -1.234},
              :key5 {:key6 :true, :key7 :false},
              :key8 {:key9 :null, :key10 {:key11 :undefined}}}}
    ;;
    ))


(deftest parse-tests-object-error-cases
  (testing-parse-error
    "parse error test cases for objects"
    {:desc "missing key before colon"
     :input (parse "{ : \"value\" }")
     :error "invalid token: [:colon] in read-pair-start-state"}
    {:desc "missing value after colon"
     :input (parse "{\"key\" : }")
     :error "invalid token: [:close-curly] in read-pair-start-state"}
    {:desc "missing colon between key value pair"
     :input (parse "{\"key\" \"value\"}")
     :error "invalid token: [:string \"value\"] in read-pair-start-state"}
    {:desc "trailing comma"
     :input (parse "{'key1': 'value1',}")
     :error "invalid token: [:close-curly] after [:comma] in read-pair-end-state"}
    {:desc "missing comma between pairs"
     :input (parse "{'key1' : 'value1' 'key2' : 'value2'}")
     :error "invalid token: [:string \"key2\"] in read-pair-end-state"}
    {:desc "extra token after }"
     :input (parse "{'key' : 'value'}, {}")
     :error "invalid token: [:comma]"}))


(deftest parse-tests-list
  (testing-is-equal
    "parse test caess for lists"
    {:desc "empty list"
     :actual (parse "{'key' : []}")
     :expect {:key []}}
    {:desc "list with single value"
     :actual (parse "{'key' : ['value']}")
     :expect {:key ["value"]}}
    {:desc "list with multiple simple values"
     :actual (parse "{'key' : ['value', 123, -1.234, true, false, null, undefined]}")
     :expect {:key ["value" 123 -1.234 :true :false :null :undefined]}}
    {:desc "list with object values"
     :actual (parse "{'key1' : [{'key2' : 'value2'}, {'key3' : 'value3'}]}")
     :expect {:key1 [{:key2 "value2"}, {:key3 "value3"}]}}
    {:desc "list with complext object structures"
     :actual (parse
               (str "{\n"
                    " 'key1' : 'value1', \n"
                    " 'key2' : [\n"
                    "    'value2',\n"
                    "    123,\n"
                    "    {\n"
                    "      'key3' : 'value3', \n"
                    "      'key4' : -1.234, \n"
                    "      'key5' : [ true, false, undefined, null ]\n"
                    "    },\n"
                    "    { 'key6' : 'value6' }\n"
                    " ]\n"
                    "}"))
     :expect {:key1 "value1",
              :key2 ["value2",
                     123,
                     {:key3 "value3", :key4 -1.234 :key5 [:true :false :undefined :null]},
                     {:key6 "value6"}]}}
    ;;
    ))
