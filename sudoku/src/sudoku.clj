
(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row col] coord]
  (into #{} (keys(frequencies(get board row))))))

(defn row-freq [board coord]
  (let [[row col] coord]
    (into #{} (vals(frequencies(get board row))))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map #(get % col) board))))

(defn col-freq [board coord]
  (let [[row col] coord]
   (into #{} (vals (frequencies (loop [c-vec [] rowind 0]
                                      (cond (= 9 rowind) c-vec
                                      :else (recur (conj c-vec (get (get board rowind) col)) (inc rowind)))))))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-help [val]
  (cond (< val 3) 0
        (< val 6) 3
        :else 6))

(defn topleft [coord]
  (let [[x y] coord]
    [(block-help x) (block-help y)]))
  
(defn block-coords [coord]
  (let [block-x [(first (topleft coord)) (+ 1 (first (topleft coord))) (+ 2 (first (topleft coord)))]
        block-y [(last (topleft coord)) (+ 1 (last (topleft coord))) (+ 2  (last (topleft coord)))]]
        (for [x block-x
              y block-y]
          [x y])))


(defn block-values [board coord]
 (into #{} (keys (frequencies (map (fn [x] (value-at board x)) (block-coords coord))))))

(defn block-freq [board coord]
  (into #{} (vals (frequencies (map (fn [x] (value-at board x)) (block-coords coord))))))


(defn valid-values-for [board coord]
  (cond (has-value? board coord) #{}
        :else (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord)))))

(defn concatboard [board]
  (loop [allboard #{}  whichvec 0]
     (cond (= whichvec 9) allboard
           :else (recur (into allboard (loop [ list () whichind 0]
                                         (cond (= 9 whichind) list
                                               :else (recur (cons (get-in board [whichvec whichind]) list) (inc whichind))))) 

                              (inc whichvec)))))
                             

(defn filled? [board]
  (not(contains? (concatboard board) 0)))

(defn rows [board]
  (loop [row-vec [] whichrow 0 col 1]
    (cond (= 9 whichrow) row-vec
          :else (recur (conj row-vec (row-values board [whichrow col ])) (inc whichrow) col))))


(defn rows-f [board]
(loop [row-vec [] whichrow 0 col 1]
  (cond (= 9 whichrow) row-vec
        :else (recur (conj row-vec (row-freq board [whichrow col ])) (inc whichrow) col))))



(defn concsets [set-vec]
  (clojure.set/union (get set-vec 0) (get set-vec 1) (get set-vec 2) (get set-vec 3) (get set-vec 4) (get set-vec 5) (get set-vec 6) (get set-vec 7) (get set-vec 8)))


(defn valid-rows? [board]
  (and (not(contains? (concsets (rows board)) 0)) (and  (not(contains? (concsets (rows-f board)) 2)) (not(contains? (concsets (rows-f board)) 3)))))

(defn cols [board]
  (loop [cols-vec [] whichcol 0 row 1]
    (cond (= 9 whichcol) cols-vec
          :else (recur (conj cols-vec (col-values board [row whichcol])) (inc whichcol) row))))

(defn cols-f [board]
  (loop [cols-vec [] whichcol 0 row 1]
    (cond (= 9 whichcol) cols-vec
          :else (recur (conj cols-vec (col-freq board [row whichcol])) (inc whichcol) row))))

  
(defn valid-cols? [board]
  (and (not(contains? (concsets (cols board)) 0)) (and  (not(contains? (concsets (cols-f board)) 2)) (not(contains? (concsets (cols-f board)) 3)))))

(def blockref [[0 0] [0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]])

(defn blocks [board]
(loop [blocks-vec [] ref 0]
  (cond (= 9 ref) blocks-vec
        :else (recur (conj blocks-vec (block-values board (get blockref ref))) (inc ref)))))

(defn blocks-f [board]
  (loop [blocks-vec [] ref 0]
    (cond (= 9 ref) blocks-vec
          :else (recur (conj blocks-vec (block-freq board (get blockref ref))) (inc ref)))))
                                                                  
(defn valid-blocks? [board]
  (and (not(contains? (concsets (blocks board)) 0)) (and  (not(contains? (concsets (blocks-f board)) 2)) (not(contains? (concsets (blocks-f board)) 3)))))

(defn valid-solution? [board]
  (and (valid-blocks? board) (and (valid-rows? board)) (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [c 0 r 0]
    (cond (not (has-value? board [r c])) [r c]
          :else (recur (inc c) (loop [c 0]
                                 (cond (not (has-value? board [r c])) c
                                       (not (= 9 c)) (inc r)
                                            :else (recur (inc c))))))))
(defn solve [board]
  nil)
