(ns metrobank.core
  (:use [clojure-csv.core])
  (:require [clojure.java.io :as io])
  (:import (org.joda.time.format DateTimeFormat)))

(defn heading-to-keyword [heading]
  (keyword (.replaceAll (.toLowerCase heading) " " "-")))

(defn header-row? [row]
  (= (first row) "Date"))

(defn load-csv [file-name]
  (let [csv-with-preamble  (parse-csv (io/reader file-name))
        csv                (drop-while (complement header-row?) csv-with-preamble)
        header             (map heading-to-keyword (first csv))]
    (map #(zipmap header %1) (rest csv))))

(defn starter-row? [row]
  (not (empty? (:date row))))

(defn merge-row [old new]
 (assoc old
        :extra-reference
        (conj (get old :extra-reference [])
              (:reference new))))

(defn combine-rows
  ([input-rows] (combine-rows input-rows nil))
  ([input-rows output-rows]
    (if (empty? input-rows)
      (reverse output-rows)
      (let [row (first input-rows)]
        (recur (rest input-rows)
               (if (starter-row? row)
                 (cons row output-rows)
                 (cons (merge-row (first output-rows) row)
                       (rest output-rows))))))))

(def headers
  ["Date" "Reference" "Paid In" "Paid Out" "Balance"])

(def input-date-formatter
  (DateTimeFormat/forPattern "d MMM yyyy"))

(def output-date-formatter
  (DateTimeFormat/forPattern "dd/MM/yyyy"))

(defn parse-date [value]
  (.parseLocalDate input-date-formatter value))

(defn format-date [value]
  (.print output-date-formatter value))

(defn empty-if-zero [value]
  (if (re-matches #"^0+(\.0+)?$" value)
    ""
    value))

(defn remove-commas [value]
  (clojure.string/replace value #"," ""))

(defn prepare-output-row [row]
  [(format-date (parse-date (:date row)))
   (:reference row)
   (empty-if-zero (remove-commas (:money-in row)))
   (empty-if-zero (remove-commas (:money-out row)))
   (remove-commas (:balance row))])

(defn -main [& args]
  (let [filename      (first args)
        cleaned-rows  (combine-rows (load-csv filename))
        csv-rows      (map prepare-output-row cleaned-rows)
        csv           (write-csv (cons headers csv-rows))]
    (println csv)))
