(ns course-material.lazy.sudoku
  (:require [clojure.string :refer [join split]]))

(def dim 3)

(def row-keys
  (memoize
    (fn [row]
      (for [column (range 1 (inc (* dim dim)))] [row column]))))

(def col-keys
  (memoize
    (fn [col]
      (for [row (range 1 (inc (* dim dim)))] [row col]))))

(def square-keys
  (memoize
    (fn [[row col]]
      (for [r (range
                (inc (* dim (quot (dec row) dim)))
                (inc (* dim (inc (quot (dec row) dim)))))
            c (range (inc (* dim (quot (dec col) dim)))
                     (inc (* dim (inc (quot (dec col) dim)))))]
        [r c]))))

(defn options-for-coordinate [[row col :as coordinate] coll]
  (into #{} (remove (set (vals (select-keys coll (concat (row-keys row)
                                                         (col-keys col)
                                                         (square-keys coordinate))))))
        (range 1 (inc (* dim dim)))))

(defn next-steps [coll]
  (when-let [nil-coordinates (seq (map key (filter (comp nil? val) coll)))]
    (let [[coordinate opts] (apply min-key (comp count second)
                                   (sequence (map (juxt identity
                                                        #(options-for-coordinate % coll)))
                                             nil-coordinates))]
      (case (count opts)
        0 nil
        1 (list (assoc coll coordinate (first opts)))
        (map #(assoc coll coordinate %) opts)))))

(defn count-nils [coll]
  (count (filter (comp nil? val) coll)))

(declare print-grid)

(defn solutions 
  ([coll] (solutions coll nil))
  ([coll sols]
   (if-let [opts (seq (next-steps coll))]
     (recur (first opts) (lazy-seq (concat (rest opts) sols)))
     (if (seq (filter nil? (vals coll)))
       (if (seq sols)
         (recur (first sols) (rest sols)))
       (lazy-seq (cons coll
                       (mapcat solutions sols)))))))

(defn print-row [row coll]
  (clojure.string/join " " (map (some-fn #(get coll %)
                                         (constantly "-"))
                                (row-keys row))))

(defn print-grid [coll]
  (doseq [row (range 1 10)]
    (println (print-row row coll)))
  (println))
