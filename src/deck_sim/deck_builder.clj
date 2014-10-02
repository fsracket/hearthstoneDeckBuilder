(ns deck-sim.deck-builder
  (:require
            [clojure.repl :as repl]
            [clojure.set :as set]
            [deck-sim.card-reader :as cards]))
  ;(:import [deck-sim.card-reader HearthStoneCard]))



(def *max-num-cards*
  "max number of non legendary cards allowed in a deck"
  2)

(defn increment-card-count [cards card-map]
  "'cards'map of maps, where each inner map has the following structure:
  {:card <HearthStoneCard> :count <Int>}.  This function increments the :count
  value of the map in cards that corresponds (i.e. is equal to) 'card-map'. A new
  of 'cards' is returned (with the updated value)."
  (let [updated-map
        (assoc-in cards [(:id (:card card-map)) :count] (+ 1 (:count card-map)))]
    updated-map))



(defn choose-random-card [player-class-cards neutral-cards choice-threshold]
  "'player-class-cards' represents all cards for a particular player class.
  'neutral-cards' represents all the neutral HearthStoneCards.  'choice-threshold'
  is a number between 0 and 1 that represents probability of choosing a player
  class card instead of a neutral card.  This function returns a random card
  chosen from one of the two sets, where the choice of set is controlled by
  'choice-threshold'."
  (let [choice-val (rand)
        [card-set size-limit] (if (< choice-val choice-threshold)
                                [player-class-cards (count player-class-cards)]
                                [neutral-cards (count neutral-cards)])]
    (nth card-set (rand-int size-limit)) ))

(def *choice-threshold*
  "A default choice threshold for use with 'choose-random-card'.  I may eventually
  parameterize this as an arbitrary 2/3 is...well...arbitrary."
  0.66)

(defn pick-random-card [player-class all-cards-by-class]
  "Given the 'player-class' and the map of all cards by class, this function
  returns a random card chosen from either the set of cards for the player-class,
  or from the neutral set.  Currently, 2/3s of the time the function will return
  a player-class card (this is hardcoded and may eventually be parameterized)."
  (let [player-class-cards (all-cards-by-class player-class)
        neutral-cards (all-cards-by-class "Neutral")]
    (choose-random-card player-class-cards neutral-cards *choice-threshold*) ))

(defn pick-cards [player-class card-set num-cards]
  "Generates a sequence of size 'num-cards' of HearthStoneCards.  The cards
  will be belong to either the 'player-class' or to the 'Neutral' set.  A maximum
  of 2 of the same card is allowed if the card is 'normal'. A maximum of 1
   is allowed if the card is 'Legendary'."
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