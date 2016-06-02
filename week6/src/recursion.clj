(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (* (first coll) (product (rest coll)))))


(defn singleton? [coll]
  (if (empty? coll) false
      (if (and (not(empty?[(first coll)])) (empty? (rest coll))) true false)))

(defn my-last [coll]
  (if (empty? coll) nil
      (if (singleton? coll) coll (last coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else
        (max (first a-seq)
              (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        :else
        (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
      (cons (if (pred? (first a-seq))
            (first a-seq)
            (my-filter pred? (rest a-seq)))
             ())))

(defn sequence-contains? [elem a-seq]
  (cond (= (first a-seq) elem) true
        (singleton? a-seq) (= elem a-seq)
        (empty? a-seq) false
        (sequence-contains? elem (next a-seq)) true
        :else false))

(defn my-take-while [pred? a-seq]
  (cond (pred? (first a-seq))
        (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        :else '()))

(defn my-drop-while [pred? a-seq]
  (cond (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))

        :else a-seq))

(defn seq= [a-seq b-seq]
  (= a-seq b-seq))

(defn my-map [f seq-1 seq-2]
  (cond (seq= seq-1 seq-2 ) (f seq-1 seq-2)
        (and (not(empty?(first seq-1))) (not(empty?(first seq-2))))
             (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
        (or (empty? seq-1) (empty? seq-2))
            (my-map f (rest seq-1) (rest seq-2))))


(defn power [n k]
  (if (zero? k) 1
      (* n (power n (dec k)))))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else  (+ (fib(- n 1)) (fib(- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond (<= how-many-times 0) ()
        :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond (< up-to 0) ()

        :else (cons (my-range (up-to)) (inc 0))))


(defn tails [a-seq]
  (cond (> (count a-seq) 0) ( a-seq  (tails(rest a-seq)))))

(defn inits [a-seq]
  [:-])

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

