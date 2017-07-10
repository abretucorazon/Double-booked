;
; Problem: Double Booked
;
; When maintaining a calendar of events, it is important to know if an event overlaps with another event.
; Given a sequence of events, each having a start and end time, write a program that will return
; the sequence of all pairs of overlapping events.
;

(ns overlapped.events
  (:require [clojure.pprint :as pp]
            [clojure.math.combinatorics :as combo]))

; Test whether 2 events do not overlap
(defn not-overlaped? [[start1 end1] [start2 end2]]
  (assert (and (< start1 end1) (< start2 end2)) "start time must be smaller than end time")
  (or (<= end1 start2) (<= end2 start1 )))

(defn overlaped? [event1 event2] (not (not-overlaped? event1 event2)))


; Create pairs of elements from two lists
(defn pair-up [lst-one lst-two]
  (map vector lst-one lst-two))

; Pair up an 'event' with each event in the list 'lst'
(defn gen-pairs [event lst]
  (pair-up (repeat (count lst) event) lst))



; ** VERSION 1**  Generate all combinations of pairs of events
(defn all-pairs [events]
   (loop [lst events result []]
       (if (empty? lst)
         result
         (let [head (first lst)
               tail (rest lst)
               pairs (gen-pairs head tail)]
             (recur tail (concat result pairs)))
          )))


; Find all pairs of overlaped events
(defn overlaps-1 [events]
  (->> events (all-pairs) (filter #(overlaped? (first %) (second %)))))




; ** VERSION 2 **  Generate all combinations of pairs of events
(defn all-tuples [events]
  (last
    (reduce (fn [[tail result] event]
              (->> tail (gen-pairs event) (concat result) (vector (rest tail))))
            [(rest events) []]
            events)))


(defn overlaps-2 [events]
  (->> events (all-tuples) (filter #(overlaped? (first %) (second %)))))




; ** VERSION 3 ** Using clojure.math.combinatorics to generate all combinations of pairs of events
(defn overlaps-3 [events]
  (let [indx-events (map-indexed #(vector %1 %2) events)                ;; Associate each event with a unique id
        indx-pairs  (combo/combinations indx-events 2)                  ;; Generate all combinations of pairs of events
        event-pairs (map (fn [[[_ x] [_ y]]] (vector x y)) indx-pairs)] ;; Drop id from every event
    (filter (fn [[x  y]] (overlaped? x y)) event-pairs)))               ;; Create sequence of overlaped pairs of events




;; *** TEST ***

; Generating events test data
(def event-list (filter #(< (first %) (last %)) (partition 2 (take 60 (repeatedly #(rand-int 12))))))


; Test version 1
(def test-ovelaps-1 (overlaps-1 event-list))

; Test version 2
(def test-ovelaps-2 (overlaps-2 event-list))

; Test version 3
(def test-overlap-3 (overlaps-3 event-list))


; compare versions  1, 2, and 3
(= test-ovelaps-1 test-ovelaps-2 test-overlap-3)


(pp/pprint test-overlap-3)
(pp/pprint (count test-overlap-3))
