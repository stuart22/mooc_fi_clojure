(ns p-p-p-pokerface)

(def suitrank {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (if (Character/isDigit (let [[rank _] card] rank))
                         (Integer/valueOf (str (let [[rank _] card] rank)))
                         (suitrank (let [[rank _] card] rank))))

(defn suit [card]
  (str(let [[_ suit] card] suit)))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (= 3 (apply max (vals (frequencies (map rank hand))))))
  (= 2 (apply min (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (and (= 2 (apply max (vals (frequencies (vals (frequencies (map rank hand)))))))
       (= 2 (apply max (keys (frequencies (vals (frequencies (map rank hand)))))))
          ))

(defn monotonic? [a-seq]
  (apply < a-seq))

(defn straight? [hand]
  (and (= 1 (apply max (vals (frequencies (map rank hand)))))
       (or (= 4 (- (apply max (map rank hand))
               (apply min (map rank hand))))
           (= 12 (- (apply max (map rank hand))
                   (apply min (map rank hand)))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (if (straight-flush? hand) 8
     (if (four-of-a-kind? hand) 7
        (if (full-house? hand) 6
            (if (flush? hand) 5
                (if (straight? hand) 4
                   (if (three-of-a-kind? hand) 3
                       (if (two-pairs? hand) 2
                           (if (pair? hand) 1 0 )))))))))
