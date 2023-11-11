#!/usr/bin/env bb

(require
 '[clojure.core.match :refer [match]]
 '[clojure.string :as s]
 '[clojure.tools.cli :refer [parse-opts]])

(def ^:private cli-options
  [["-u" "--unique" "Remove duplicates from the inputs"]
   ["-s" "--sort SORT" "Select algorithm to use"
    :id :algorithm
    :default "default"
    :parse-fn s/lower-case]
   ["-R" "--random-sort" "Sort by a random order"]])

(def ^:private RANDOM "random")

(defmulti ^:private sort-lines
  (fn [_ sort-option] sort-option))

(defmethod ^:private sort-lines :default
  [lines _]
  (sort lines))

(defmethod ^:private sort-lines RANDOM
  [lines _]
  (shuffle lines))

(defn- read-input
  [arguments]
  {:pre [(let [cnt (count arguments)] (<= 0 cnt 1))]}
  (->>
   (match arguments
     [] *in*
     ["-"] *in*
     [filename] filename)
   slurp))

(defn -main
  [& args]
  (try
    (let [arg-map (parse-opts args cli-options)
          {:keys [unique random-sort]} (:options arg-map)
          algorithm (if random-sort RANDOM (-> arg-map :options :algorithm))]
      (->> (read-input (:arguments arg-map))
           (#(s/split % #"\n"))
           (#(if unique (into #{} %) %))
           (#(sort-lines % algorithm))
           (#(doall (map println %)))))
    (catch Exception _
      (.println *err* "Invalid arguments"))))

(apply -main *command-line-args*)
