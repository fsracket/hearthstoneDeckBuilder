(ns deck-sim.card-reader
  (:require [clojure.data.json :as json]
            [clojure.repl :as repl]
            [clojure.set :as set]))


(def ^:dynamic *card-json*
          (json/read-str
            (slurp (.getPath (clojure.java.io/resource "AllSets.json")))
            :key-fn #(keyword (.toLowerCase (.replaceAll % " " ""))) ))

(def excluded-sets #{:system :debug :promotion})

(def *retained-card-sets* (set/difference (into #{} (keys *card-json*)) excluded-sets))

(def ^:dynamic *all-cards* (flatten (map #(% *card-json*) *retained-card-sets*)))

(def *card-mechanics* (remove nil? (into #{} (flatten (map :mechanics *all-cards*)))))

(def all-card-attrs (into #{} (flatten (map keys *all-cards*))))

(def card-record-attrs (into [] (sort (map (comp symbol name) all-card-attrs))))
(def card-keys (map keyword card-record-attrs))

(defmacro make-card-record [record-name names]
  (let [syms# (eval names)] ;the use of eval is clearly not right
    `(defrecord ~record-name [~@syms#])))




;;I stole these 2 functions from somewhere; don't remember where
(defn- static? [field]
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

(make-card-record HearthStoneCard card-record-attrs)


;;create a Card record for each instance of card map from the json

(defn card-vals [card-map-inst card-keys]
  (->> card-keys
       (map #(% card-map-inst))))

(defn make-cards [card-maps card-keys]
  (let [card-val-lists (map #(card-vals % card-keys) card-maps)]
    (flatten (map #(apply ->HearthStoneCard %) card-val-lists ))))

(def all-hearthstone-cards (make-cards *all-cards* card-keys))

(def all-cards-by-class
  (group-by
    (fn [card]
      (if-let [player-class (:playerclass card)]
        player-class
        "Neutral"))
    all-hearthstone-cards))
