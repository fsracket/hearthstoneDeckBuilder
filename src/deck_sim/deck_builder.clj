(ns deck-sim.deck-builder
  (:require
            [clojure.repl :as repl]
            [clojure.set :as set]
            [deck-sim.card-reader :as cards]))
  ;(:import [deck-sim.card-reader HearthStoneCard]))

(nth cards/all-hearthstone-cards 12)

(defrecord CardSelection [id card-count-map])

(defn card-selected? [id selected-cards]
  (contains? selected-cards id))

(defn available-cards [player-class card-set]
  (filter #(or (nil? (:playerclass %))
               (= (:playerclass %) player-class))
          card-set))

;
;(loop [match (re-find matcher) ;loop starts with 2 set arguments
;       result []]
;  (if-not match
;    result
;    (recur (re-find matcher)    ;loop with 2 new arguments
;           (conj result match))))))



(def *max-num-cards* 2)

(defn increment-card-count [cards card-map]
  (let [updated-map
        (assoc-in cards [(:id (:card card-map)) :count] (+ 1 (:count card-map)))]
    updated-map))



(defn choose-random-card [player-class-cards neutral-cards choice-threshold]
  (let [choice-val (rand)
        [card-set size-limit] (if (< choice-val choice-threshold)
                                [player-class-cards (count player-class-cards)]
                                [neutral-cards (count neutral-cards)])]
    (nth card-set (rand-int size-limit)) ))

(def *choice-threshold* 0.66)

(defn pick-random-card [player-class all-cards-by-class]
  (let [player-class-cards (all-cards-by-class player-class)
        neutral-cards (all-cards-by-class "Neutral")]
    (choose-random-card player-class-cards neutral-cards *choice-threshold*) ))

(defn pick-cards [player-class card-set num-cards]


    (loop [selected-cards {}
           num-to-select num-cards]
      (let [candidate-card (pick-random-card player-class card-set)
            existing-card-entry (selected-cards (:id candidate-card) )]
        ;(println (str (:id candidate-card) " ,count: " (:count existing-card-entry)))
        ;(println num-to-select)
        (cond
          (= num-to-select 0) selected-cards
          (and existing-card-entry
               (= (:rarity existing-card-entry) "Legendary"))
          (recur selected-cards num-to-select) ;only 1 allowed
          (and existing-card-entry
               (< (:count existing-card-entry) *max-num-cards*))  ;2 allowed
          (recur (increment-card-count selected-cards existing-card-entry)
                 (- num-to-select 1))
          (not existing-card-entry) ;add card if not in deck
          (recur (assoc selected-cards
                   (:id candidate-card)
                   {:card candidate-card :count 1})
                 (- num-to-select 1))
          :else (recur selected-cards num-to-select) ;pick a different card; count stays the same
          )) ))