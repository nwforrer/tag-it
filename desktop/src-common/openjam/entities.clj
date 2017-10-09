(ns openjam.entities
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.math :refer :all]
            [openjam.utils :as u]))

(defn create-player
  [screen stand jump spray & walk]
  (let [object (map-objects! (map-layer!
                              (map-layer screen "player")
                              :get-objects) :get 0)
        props (map-object! object :get-properties)
        direction (if (= (.get props
                               "direction") "right")
                    :right
                    :left)
        x (/ (rectangle! (map-object! object :get-rectangle) :get-x) u/pixels-per-tile)
        y (/ (rectangle! (map-object! object :get-rectangle) :get-y) u/pixels-per-tile)]
    (assoc stand
           :stand-right stand
           :stand-left (texture stand :flip true false)
           :jump-right jump
           :jump-left (texture jump :flip true false)
           :spray-right spray
           :spray-left (texture spray :flip true false)
           :walk-right (animation u/duration
                                  walk
                                  :set-play-mode (play-mode :loop-pingpong))
           :walk-left (animation u/duration
                                 (map #(texture % :flip true false) walk)
                                 :set-play-mode (play-mode :loop-pingpong))
           :width 1
           :height 1;;(/ 26 18)
           :x-velocity 0
           :y-velocity 0
           :x x
           :y y
           :me? true
           :spraying? false
           :can-jump? false
           :direction direction)))

(defn create-police
  [screen stand & walk]
  (let [objects (map-layer!
                 (map-layer screen "enemies")
                 :get-objects)]
    (map (fn [object]
           (let [props (map-object! object :get-properties)
                 direction (if (= (.get props
                                   "direction") "right")
                             :right
                             :left)
                 sight (.get props "sight")
                 x-velocity (if (= direction :right)
                              u/max-ai-velocity
                              (* -1 u/max-ai-velocity))]
             (assoc stand
                    :stand-right stand
                    :stand-left (texture stand :flip true false)
                    :walk-right (animation u/duration
                                           walk
                                           :set-play-mode (play-mode :loop-pingpong))
                    :walk-left (animation u/duration
                                          (map #(texture % :flip true false) walk)
                                          :set-play-mode (play-mode :loop-pingpong))
                    :width 1
                    :height 1
                    :x-velocity x-velocity
                    :y-velocity 0
                    :x (/ (rectangle! (map-object! object :get-rectangle) :get-x) u/pixels-per-tile)
                    :y (/ (rectangle! (map-object! object :get-rectangle) :get-y) u/pixels-per-tile)
                    :can-jump? false
                    :direction direction
                    :sight (read-string sight))))
         objects)))

(defn move
  [{:keys [delta-time]} {:keys [x y me? can-jump?] :as entity}]
  (let [x-velocity (u/get-x-velocity entity)
        y-velocity (+ (u/get-y-velocity entity) u/gravity)
        x-change (* x-velocity delta-time)
        y-change (* y-velocity delta-time)]
    (if (or (not= 0 x-change) (not= 0 y-change))
      (assoc entity
             :x-velocity (u/decelerate me? x-velocity)
             :y-velocity (u/decelerate me? y-velocity)
             :x-change x-change
             :y-change y-change
             :x (+ x x-change)
             :y (+ y y-change)
             :can-jump? (if (> y-velocity 0) false can-jump?))
      entity)))

(defn animate
  [screen {:keys [me?
                  x-velocity y-velocity
                  stand-right stand-left
                  jump-right jump-left
                  spray-right spray-left
                  walk-right walk-left] :as entity}]
  (let [direction (u/get-direction entity)]
    (merge entity
           (cond
             (not= y-velocity 0)
             (if (= direction :right) jump-right jump-left)
             (not= x-velocity 0)
             (if (= direction :right)
               (animation->texture screen walk-right)
               (animation->texture screen walk-left))
             (and me? (key-pressed? :enter))
             (if (= direction :right)
               spray-right
               spray-left)
             :else
             (if (= direction :right) stand-right stand-left))
           {:direction direction})))

(defn prevent-move
  [screen {:keys [x y x-change y-change x-velocity me?] :as entity}]
  (let [old-x (- x x-change)
        old-y (- y y-change)
        entity-x (assoc entity :y old-y)
        entity-y (assoc entity :x old-x)
        up? (> y-change 0)]
    (merge entity
           (when (or (u/get-touching-tile screen entity-x "walls" "building"))
             {:x-velocity 0 :x-change 0 :x old-x})
           (when-let [tile (or (u/get-touching-tile screen entity-y "walls" "building"))]
             {:y-velocity 0 :y-change 0 :y old-y
              :can-jump? (not up?) :to-destroy false})
           (if (and (not me?) (u/get-touching-tile screen entity-x "turns"))
             (if (= direction :left)
               {:direction :right :x-velocity (* -1 x-velocity)}
               {:direction :left :x-velocity (* -1 x-velocity)}))
           (when (<= x 0)
             {:x-velocity 0 :x 0})
           (when (>= x 49)
             {:x-velocity 0 :x 49}))))
