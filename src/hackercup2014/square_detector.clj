(ns hackercup2014.square-detector

  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn to-int [s]
  (Integer/parseInt s))

(defn to-squares [input]
  (if-let [f (first input)]
    (cons (take (to-int f) (rest input))
          (lazy-seq (to-squares (drop (to-int f) (rest input)))))
    '()))

(defn square? [s]
  (let [size (count s)
        filled (for [y (range size)
                     x (range size)
                     :when  (= \# (nth (nth s y)
                                       x))]
                 [y x])
        ys (map first filled)
        xs (map second filled)
        x-min (apply min xs)
        x-max (apply max xs)
        y-min (apply min ys)
        y-max (apply max ys)
        y-edge (- y-max (dec y-min))
        x-edge (- x-max (dec x-min))
        square-size (* y-edge x-edge)]
    (if (and (= x-edge y-edge)
             (= (count filled) square-size))
      "YES"
      "NO")))

(defn process-file [f input output]
  (with-open [output-file (io/writer output)
              input-file (io/reader input)]
    (doseq [output-line
            (map (partial format "Case #%d: %s\n" )
                 (iterate inc 1)
                 (pmap f (to-squares (drop 1 (line-seq input-file)))))]
        (do (.write output-file output-line)
            (print output-line)))))

(defn -main []
  (process-file square?
                "square_detector_input.txt"
                "square_detector_output.txt")
  (shutdown-agents))
