(ns iloveponies.tests.training-day
  (:use midje.sweet
        training-day))

(facts "answer" {:exercise 5
                 :points 1}
  answer => 42)

(facts "hai" {:exercise 6
               :points 1}
  hai => "OH HAI!")

(facts "square" {:exercise 7
                 :points 1}
  (square 2) => 4
  (square 3) => 9)

(facts "average" {:exercise 8
                  :points 1}
  (average 2 4) => 3
  (average 1 2) => 3/2)
