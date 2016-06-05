(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* base acc) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (= a-seq []) nil
    (let [helper (fn [acc a-seq]
                 (cond (empty? a-seq) acc
                       :else (recur (first a-seq) (rest a-seq))))]
    (helper [] a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond (not= (first seq1) (first seq2))
                         false
                       (not= (count seq1) (count seq2))
                         false
                       (and (empty? seq1) (empty? seq2))
                         true
                       :else (recur (rest seq1) (rest seq2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [ind 0 my-seq a-seq]
                  (cond (= (count a-seq) ind)
                          nil
                        (pred (get my-seq ind))
                          ind
                        :else (recur (inc ind) a-seq))))

(defn avg [a-seq]
  (loop [sum 0 nums a-seq ind 0 ]
    (cond (= ind (count a-seq))
          (/ sum ind)
          :else
          (recur (+ sum (first nums)) (rest nums) (inc ind)))))

(defn toggle [a-set elem]
  (if (= (conj a-set elem) a-set)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [parity-set #{}
         my-seq a-seq]
    (if (empty? my-seq)
      parity-set
      (recur (toggle parity-set (first my-seq))
             (rest my-seq)))))

(defn fast-fibo [n]
  (loop [ x [0 1]]
    (cond (= 0 n) 0
          (< (count x) n) (recur (conj x (+ (last x) (nth x (- (count x) 2)))))
          :else (+ (last x) (last (butlast x))))))



(defn cut-at-repetition [a-seq]
  (loop [seen-set #{}
         my-seq a-seq
         singles-seq []]
    (let [next-key (first my-seq)]
      (cond (contains? seen-set next-key) singles-seq
            (nil? next-key) singles-seq
            :else (recur (conj seen-set next-key)
                         (rest my-seq)
                         (conj singles-seq next-key))))))

