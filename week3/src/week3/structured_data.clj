(ns week3.structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
       (Math/pow xx xx)))


(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3" ))

(defn spiff-destructuring [v]
  (let [[x y z] v]
      (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
      (let [[[x1 y1] [x2 y2]] rectangle]
           (- x2 x1)))

(defn height [rectangle]
      (let [[[x1 y1] [x2 y2]] rectangle]
           (- y2 y1)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
     (and (<= x1 x x2 ) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
      (let [[[x1_out y1_out] [x2_out y2_out]] outer
             [[x1_in y1_in] [x2_in y2_in]] inner]

        (and (and (<= x1_out x1_in) (<= x2_in x2_out))
                 (and (<= y1_out y1_in) (<= y2_in y2_out))))

        )

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
      (count (:authors book)))

(defn multiple-authors? [book]
      (<= 2 (count (:authors book))))

(defn add-author [book new-author]
      (assoc book :authors (conj (:authors book) new-author) ))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn count-el [element]
  (count element))

(defn element-lengths [collection]
  (map count-el collection))

(defn get-sec [collection]
(first (rest collection)))

(defn second-elements [collection]
  (map get-sec collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or(apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (clojure.string/join (repeat n "*")))

(defn toggle [a-set elem]
    (if (= (conj a-set elem) a-set)
      (disj a-set elem)
      (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count (distinct a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (into #{} (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (into #{} (map :name (authors books))))

(defn author->string [author]
  (if (contains? author :death-year)
    (str (:name author) " ("(:birth-year author) " - " (:death-year author) ")" )
    (if (contains? author :birth-year)
      (str (:name author) " ("(:birth-year author) " - " ")" )
      (:name author))))



(defn authors->string [authors]
   (clojure.string/join  ", "  (map author->string authors)))

(defn book->string [book]
  (clojure.string/join
    (str (:title book) ", written by " (authors->string (:authors book)))))

(defn books->string [books]
  (if (= books [])
    (str "No books.")
    (str (count books)
         (if (= 1 (count books)) " book. " " books. ")
         (clojure.string/join ", " (map book->string books)) ".")))

(defn books-by-author [author books]
  (filter  (fn [book] (has-author? book author)) books))


(defn is-author [name authors]
  (.contains (map :name authors) name))


(defn author-by-name [name authors]
  (map (.contains (map :name authors) name)) name authors)

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  :-)

; %________%
