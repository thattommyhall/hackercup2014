(ns hackercup2014.tennison
  (:use [hackercup2014.core]
        [criterium.core])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.reducers :as r]))
(set! *warn-on-reflection* true)

(defn normalise [p]
  (cond (< p 0.0)
        0.0
        (> p 1.0)
        1.0
        :else
        p))

(defn sim
  ([k ps pr pi pu pw pd pl]
     (sim (int k) ps pr pi pu pw pd pl 0 0))
  ([k ps pr pi pu pw pd pl wins losses]
     (cond (= k wins)
           1.0

           (= k losses)
           0.0
           ;; ps sunny win
           ;; pr rain win
           ;; pi chance of sun
           ;; wins: +pu with prob pw
           ;; lose: -pd with prob pl

           :else
           (let [pwin (if (< (rand) pi) ps pr)
                 won? (< (rand) pwin)]
             (if won?
               (let [increase? (< (rand) pw)
                     delta (if increase? pu 0)]
                 (recur k ps pr (min (+ delta pi) 1) pu pw pd pl (inc wins) losses))
               (let [decrease? (< (rand) pl)
                     delta (if decrease? pd 0)]
                 (recur k ps pr (max (- pi delta) 0) pu pw pd pl wins (inc losses))))))))

(defn win-probability [& args]
  (let [n 1000000
        won (reduce +
                    (pmap (fn [n]
                            (apply sim args))
                          (range n)))]
    (/ won n)))


(defn win-probability2 [k ps pr pi pu pw pd pl]
  (let [lookup (atom {})
        F
        (fn F  [w l p]
          (cond (== w k)
                1
                (== l k)
                0
                :else
                (if-let [cached (@lookup [w l p])]
                  cached
                  (let [answer
                        (+
                         (* (normalise p) ps pw (F (inc w) l (+ (normalise p) pu)))
                         (* (normalise p) ps (- 1 pw) (F (inc w) l (normalise p)))
                         (*  (- 1 ps) pl (F w (inc l) (- (normalise p) pd)))
                         (* (normalise p) (- 1 ps) (- 1 pl) (F w (inc l) (+ (normalise p) pu)))
                         (* (- 1 (normalise p)) pr pw (F (inc w) l (+ (normalise p) pu)))
                         (* (- 1 (normalise p)) pr (- 1 pw) (F (inc w) l (normalise p)))
                         (* (- 1 (normalise p)) (- 1 pr) pl (F w (inc l) (- (normalise p) pd)))
                         (* (- 1 (normalise p)) (- 1 pr) (- 1 pl) (F w (inc l) (normalise p))))]
                    (swap! lookup assoc [w l p] answer)
                    answer))))]
    (F 0 0 pi)))

;; (defn f [line]
;;   (println "Running " line)
;;   (let [[k ps pr pi pu pw pd pl]
;;         (map #(Double/parseDouble %) (str/split line #" "))]
;;     (win-probability k ps pr pi pu pw pd pl)))

(defn f [line]
  (let [[k ps pr pi pu pw pd pl]
        (map #(Double/parseDouble %) (str/split line #" "))]
    (win-probability2 k ps pr pi pu pw pd pl)))


(def eg1 [1.0 0.800 0.100 0.500 0.500 0.500 0.500 0.500])
(def eg2 [2.0 0.600 0.200 0.500 0.100 1.000 0.100 1.000])
(def eg3 [1.0 1.000 0.000 1.000 1.000 1.000 1.000 1.000])
(def eg4 [25.0 0.984 0.222 0.993 0.336 0.207 0.084 0.478])
(def eg5 [58.0 0.472 0.182 0.418 0.097 0.569 0.816 0.711])


(defn -main []
  (doseq [eg [eg1 eg2 eg3 eg4 eg5]]
    (println eg)
    (println (apply win-probability2 eg)))
  ;; (shutdown-agents)
  )
