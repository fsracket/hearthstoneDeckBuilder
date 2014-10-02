(ns deck-sim.card-reader
  (:require [clojure.data.json :as json]
            [clojure.repl :as repl]
            [clojure.set :as set]))



(def ^:dynamic *card-json*
  "The sequence of nested maps where each map represents a set of cards (basic, expert, etc)"
          (json/read-str
            (slurp (.getPath (clojure.java.io/resource "AllSets.json")))
            :key-fn #(keyword (.toLowerCase (.replaceAll % " " ""))) ))

(def excluded-sets #{:system :debug :promotion})

(def ^:dynamic *retained-card-sets* (set/difference (into #{} (keys *card-json*)) excluded-sets))

(def ^:dynamic *all-cards* (flatten (map #(% *card-json*) *retained-card-sets*)))

(def ^:dynamic *card-mechanics*
  "the unique set of card mechanics"
  (remove nil? (into #{} (flatten (map :mechanics *all-cards*)))))

(def all-card-attrs
  "The unique set of attributes that are defined across all cards.  A given
  card will likely only have values for some subset of these attributes."
  (into #{} (flatten (map keys *all-cards*))))


(def card-record-attrs
  "represents attributes as symbols so that they can be passed into a 'defrecord' call"
  (into [] (sort (map (comp symbol name) all-card-attrs))))
(def card-keys (map keyword card-record-attrs))


(defmacro make-card-record [record-name names]
  "Just a small helper to allow for the list of field-names for the 'defrecord'
  form to be dynamic.  The normal 'defrecord' form doesn't accept a vector."
  (let [syms# (eval names)] ;the use of eval is clearly not right
    `(defrecord ~record-name [~@syms#])))




;;I stole these 2 functions from somewhere; don't remember where
(defn- static? [field]
  "Returns true if 'field' is a static member (of whatever class it belongs to)."
  (java.lang.reflect.Modifier/isStatic
    (.getModifiers field)))

(defn fields [record-type]
  "Returns a list of all the keyword accessors for the specified record-type."
  (->> record-type
       .getDeclaredFields
       (remove static?)
       (map #(.getName %))
       (remove #{"__meta" "__extmap"})
       (map keyword)))

;;generates a record to represent each HearthStoneCard
;;this record will probably be used only as a map, but
;;if there's ever a future need to do Protocols, having a specific
;;'HearthStoneCard' type will make the function signatures more readable (I think)
(make-card-record HearthStoneCard card-record-attrs)


;;create a Card record for each instance of card map from the json

(defn card-vals [card-map-inst card-keys]
  "'card-map-inst' is the map of information for a given HearthStone card
  that was originally extracted form the json.  This function takes the keywords
  defined in 'card-keys' and generates the list of values that result from using
  each keyword as an 'accessor' for 'card-map-inst'.  Using the 'keyword accessors'
  in this manner will return nil when the keyword is not a defined key for 'card-map-inst'."
  (->> card-keys
       (map #(% card-map-inst))))

(defn make-cards [card-maps card-keys]
  "Generates an instance of 'HearthStoneCard' for all cards that
  we want to work with."
  (let [card-val-lists (map #(card-vals % card-keys) card-maps)]
    (flatten (map #(apply ->HearthStoneCard %) card-val-lists ))))


(def all-hearthstone-cards
  "The list of all HearthStoneCard records."
  (make-cards *all-cards* card-keys))

(def all-cards-by-class
  "Generates a map of all HearthStoneCards where each key in the map represents
  a player class (e.g. Rogue, Hunter, Druid, etc), and where each value is a
  list of HearthStoneCard records.  If the card has a 'nil' value for player class,
  it is placed in a 'Neutral' group (i.e. 'Neutral' is th key)."
  (group-by
    (fn [card]
      (if-let [player-class (:playerclass card)]
        player-class
        "Neutral"))
    all-hearthstone-cards))
