(ns openjam.utils
  (:require [play-clj.core :refer :all]))

(def ^:const vertical-tiles 20)
(def ^:const horizontal-tiles 50)
(def ^:const pixels-per-tile 64)
(def ^:const duration 0.15)
(def ^:const damping 0.5)
(def ^:const max-velocity 14)
(def ^:const max-ai-velocity 1)
(def ^:const max-jump-velocity (* max-velocity 4))
(def ^:const deceleration 0.9)
(def ^:const gravity -2.5)
(def ^:const level1 "level1.tmx")

(def current-level (atom level1))
(def show-intro (atom true))

(defn decelerate
  [me? velocity]
  (if me? (let [velocity (* velocity deceleration)]
            (if (< (Math/abs velocity) damping)
              0
              velocity))
      velocity))

(defn get-x-velocity
  [{:keys [me? x-velocity]}]
  (if me?
    (cond
      (key-pressed? :dpad-left)
      (* -1 max-velocity)
      (key-pressed? :dpad-right)
      max-velocity
      :else
      x-velocity)
    x-velocity))

(defn get-y-velocity
  [{:keys [me? y-velocity can-jump?]}]
  (if me?
    (cond
      (and can-jump? (key-pressed? :dpad-up))
      max-jump-velocity
      :else
      y-velocity)
    y-velocity))

(defn get-direction
  [{:keys [x-velocity direction]}]
  (cond
    (> x-velocity 0) :right
    (< x-velocity 0) :left
    :else
    direction))

(defn get-touching-tile
  [screen {:keys [x y width height]} & layer-names]
  (let [layers (map #(tiled-map-layer screen %) layer-names)]
    (->> (for [tile-x (range (int x) (+ x width))
               tile-y (range (int y) (+ y height))]
           (some #(when (tiled-map-cell % tile-x tile-y)
                    [tile-x tile-y])
                 layers))
         (drop-while nil?)
         first)))
