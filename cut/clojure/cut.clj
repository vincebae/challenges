#!/usr/bin/env bb

(require '[clojure.core.match :refer [match]]
         '[clojure.string :as s]
         '[clojure.tools.cli :refer [parse-opts]])

(def ^:private cli-options
  [["-f" "--fields FIELDS" "Field number"
    :default nil
    :parse-fn (fn [input]
                (->> (s/split input #"[,| ]+")
                     (map #(Integer/parseInt %))
                     (map dec)
                     set))]
   ["-d" "--delim DELIMITER" "Delimiter"
    :default (str \tab)]])

(defn- read-input
  [arguments]
  (->>
   (match arguments
     [] *in*
     ["-"] *in*
     [filename] filename
     :else nil)
   slurp))

(defn- cut-line
  [line fields delim]
  (->> (s/split line (re-pattern delim))
       (map vector (iterate inc 0))
       (keep (fn [[idx text]] (when (fields idx) text)))
       (s/join delim)))

(defn- cut
  [text options]
  (let [fields (->> options :fields)
        delim (->> options :delim)]
    (->> text
         (#(s/split % #"\n"))
         (map #(cut-line % fields delim)))))

(defn -main
  [& args]
  (try
    (let [arg-map (parse-opts args cli-options)
          text (read-input (:arguments arg-map))]
      (when text
        (->> (cut text (:options arg-map))
             (#(doall (map println %))))))
    (catch Exception _
      (.println *err* "Invalid arguments"))))

(apply -main *command-line-args*)
