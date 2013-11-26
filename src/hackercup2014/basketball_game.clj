(ns hackercup2014.basketball-game
  (:use hackercup2014.core)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn to-int [s]
  (Integer/parseInt s))

(defn parse-input [input]
  (if-let [f (first input)]
    (let [[n m p] (str/split f #" ")
          m (to-int m)
          n (to-int n)
          p (to-int p)]
      (cons [m p (take n (rest input))]
            (lazy-seq (parse-input (drop (to-int (str n)) (rest input))))))
    '()))

(defn parse-player [player]
  (let [[name p height] (str/split player #" ")]
    [name (to-int p) (to-int height)]))

(defn compare-players
  [[name p height] [name' p' height']]
  (if (= p p')
    (> height height')
    (> p p')))

(def eg [93 2
         ["Paul 82 189"
          "Kittipat 62 126"
          "Thomas 17 228"
          "Fabien 57 233"
          "Yifei 65 138"
          "Liang 92 100"
          "Victor 53 124"]])

(defn sort-players [players]
  (map first
       (apply sorted-set-by
              compare-players
              players)))

(defn f [[m p players]]
  (let [players (sort-players (map parse-player players))
        team1 (take-nth 2 players)
        team2 (take-nth 2 (rest players))
        mth (fn [team]
              (let [team-size (count team)]
                (nth (partition p 1  (cycle (concat (reverse (take p team))
                                                    (drop p team))))
                     m)))]

    (str/join " " (sort (concat (mth team1)
                                (mth team2))))))

(defn process-file [f parser input output]
  (with-open [output-file (io/writer output)
              input-file (io/reader input)]
    (doseq [output-line
            (map (partial format "Case #%d: %s\n" )
                 (iterate inc 1)
                 (pmap f (parser (drop 1 (line-seq input-file)))))]
      (do (.write output-file output-line)
          (print output-line)))))

(defn -main []
  (process-file
   f
   parse-input
   "basketball_game_input.txt"
   "basketball_game_output.txt")
  (shutdown-agents))
