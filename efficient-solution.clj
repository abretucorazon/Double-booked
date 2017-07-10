;
; Problem: Double Booked
;
; When maintaining a calendar of events, it is important to know if an event overlaps with another event.
; Given a sequence of events, each having a start and end time, write a program that will return
; the sequence of all pairs of overlapping events.
;
; Input:  [[s1 e1] [s2 e2]...]
; Output: [[[sx ex] [sy ey]] [[sa ea] [sb eb]]...]
;
;
(ns double.booked
  (:require [clojure.pprint :as pp]
            [clojure.math.combinatorics :as combo]
            [clojure.test :refer-macros [deftest is]]))


; **** A MORE EFFICIENT SOLUTION
;
; 1. there are 24 hours in a day - we want to create 24 lists, one for each hour of the day
;
(def hour-map (zipmap (range 0 24) (repeat 24 [])))

; 2. add each event in the list that correspond to the hours that event will take place e.g.
;    event [3 5] will be added to the lists for 3 and 4 o'clock.
;    event [9 12] will be added to the lists for 9, 10, 11 o'clock
;
(defn add-event [hour-map [start end :as event]]
  (reduce (fn [hour-map hour]
            (as-> hour x (hour-map x) (conj x event) (assoc hour-map hour x)))
           hour-map
          (range start end)))

(defn add-event-seq [hour-map events]
  (reduce (fn [hour-map event] (add-event hour-map event))
          hour-map
          events))

; 3. of the 24 lists remove those that contain less than 2 events
;
(defn del-non-overlaps [hour-map]
  (filter (fn [[hour events]] (<= 2 (count events))) hour-map))


; 4. generate all possible pairs of events separately for each of the remaining lists -
;    conjoin all the 0 to 24 lists of overlaped events into one big set -
;
(defn overlapped-pairs [hour-map]
    (reduce (fn [overlaps events ] (apply conj overlaps (combo/combinations events 2)))
             #{}
            (vals hour-map)))


; 5. Append an unique id to each event -
;    Generate all overlapped pairs of events
;
(defn label-events [events]
    (map-indexed (fn [idx event] (conj (vec event) (keyword (str "ev" idx)))) events))


(defn double-booked [events]
  (->> events
       (label-events)
       (add-event-seq hour-map)
       (del-non-overlaps)
       (overlapped-pairs)
       (seq)
       ))



(defonce event-list (filter #(< (first %) (last %)) (partition 2 (take 60 (repeatedly #(rand-int 24))))))

(pp/pprint (label-events event-list))

(pp/pprint (set (double-booked event-list)))


;
; *** TEST ***
;


; *** A simple inefficient solution ***
;

; Test whether 2 events do not overlap
(defn not-overlaped? [[start1 end1] [start2 end2]]
  (assert (and (< start1 end1) (< start2 end2)) "start time must be smaller than end time")
  (or (<= end1 start2) (<= end2 start1 )))

; Test if 2 events overlap
(defn overlaped? [event1 event2] (not (not-overlaped? event1 event2)))


; Using clojure.math.combinatorics to generate sequence of all pairs of events
(defn gen-all-pairs [events]
  (let [indx-events (label-events events)                               ;; Associate each event with a unique id
        indx-pairs  (combo/combinations indx-events 2)                  ;; Generate all combinations of pairs of events
        ]
    indx-pairs))

;; Return sequence of all overlaped pairs of events
(defn all-overlaps [events]
    (filter (fn [[x  y]] (overlaped? x y)) (gen-all-pairs events)))


(pp/pprint "ALL-OVERLAPS algorithm")
(pp/pprint (label-events event-list))

(pp/pprint (set (all-overlaps event-list)))

(deftest coverage

  (refer 'clojure.set)

  ; every pairs of events in sequence is overlaped
  (is (every? (fn [[ev1 ev2]] (overlaped? ev1 ev2))
              (double-booked event-list)) "every pairs of events in sequence is overlaped")

  ; cross-check result with another algorithm
  (is (= (set (double-booked event-list)) (set (all-overlaps event-list))) "cross-check results between double-booked & all-overlaps")

  ; all remaining events are  non-overlaped
  (is (let [overlaps     (set (double-booked event-list))
            all-pairs    (set (gen-all-pairs event-list))
            non-overlaps (seq (difference overlaps all-pairs))]
         (every? (fn [[ev1 ev2]] (not-overlaped? ev1 ev2)) non-overlaps)))

  ; number of pairs of events = (number of overlaps + number of non-overlaps)
  (is (let [all-pairs    (gen-all-pairs event-list)
            overlaps     (double-booked event-list)
            non-overlaps (difference (set all-pairs ) (set overlaps))]
         (= (count all-pairs) (+ (count overlaps) (count non-overlaps)))))

)

(test/run-tests)

;(pp/pprint test-overlap-1)

