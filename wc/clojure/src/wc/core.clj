#!/usr/bin/env bb

;; Coding challenge for wc tool.
;; https://codingchallenges.fyi/challenges/challenge-wc
;; Written for bb script without cli library

(ns wc.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]))

(def opts-arg-map
  {"-c" [:show :bytes], "--bytes" [:show :bytes],
   "-m" [:show :chars], "--chars" [:show :chars],
   "-w" [:show :words], "--words" [:show :words],
   "-l" [:show :lines], "--lines" [:show :lines]})

(def default-opts [:lines :words :bytes])
(def all-opts [:lines :words :chars :bytes])

(defn make-vector
  [value]
  (cond
    (nil? value) []
    (coll? value) (vec value)
    :else [value]))

(defn add-arg
  [arg-map arg value]
  (->> (get arg-map arg)
       (make-vector)
       (#(conj % value))
       (assoc arg-map arg)))

(defn parse-args
  ([args] (parse-args args {}))
  ([args arg-map]
   (if-let [[x & xs] (seq args)]
     (->> (get opts-arg-map x [:filename x])
          (apply add-arg arg-map)
          (recur xs))
     arg-map)))

(defn get-counts
  [text]
  {:bytes (count (.getBytes text))
   :chars (count text)
   :words (if (empty? text) 0 (count (s/split text #"\s+")))
   :lines (reduce #(if (= %2 \newline) (inc %1) %1) 0 text)})

(defn get-results
  [text show-opts]
  (let [counts (get-counts text)
        show-opts-ordered (filter (into #{} show-opts) all-opts)]
    (-> (map counts show-opts-ordered)
        (make-vector))))

(defn wc-by-reader
  [reader show-opts]
  (->> (slurp reader)
       (#(get-results % show-opts))
       (s/join " ")))

(defn -main
  [args]
  (let* [arg-map (parse-args args)
         show-opts (get arg-map :show default-opts)]
        (if (:filename arg-map)
          (doseq [filename (:filename arg-map)]
            (try
              (with-open [file-reader (io/reader filename)]
                (println (wc-by-reader file-reader show-opts) filename))
              (catch Exception e
                (println (.getMessage e)))))
          (println (wc-by-reader *in* show-opts)))))
