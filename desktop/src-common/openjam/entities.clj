(ns openjam.entities
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [openjam.utils :as u]))

(defn create-player
  [stand jump & walk]
  (assoc stand
         :stand-right stand
         :stand-left (texture stand :flip true false)
         :jump-right jump
         :jump-left (texture jump :flip true false)
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
         :x 20
         :y 2
         :me? true
         :can-jump? false
         :direction :right))

(defn create-police
  [stand jump & walk]
  (assoc stand
         :stand-right stand
         :stand-left (texture stand :flip true false)
         :jump-right jump
         :jump-left (texture jump :flip true false)
         :walk-right (animation u/duration
                                walk
                                :set-play-mode (play-mode :loop-pingpong))
         :walk-left (animation u/duration
                               (map #(texture % :flip true false) walk)
                               :set-play-mode (play-mode :loop-pingpong))
         :width 1
         :height 1;;(/ 26 18)
         :x-velocity (* 1 u/max-ai-velocity)
         :y-velocity 0
         :x 15
         :y 10
         :can-jump? false
         :direction :right))

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

(defn spray
  [screen {:keys [x me?] :as entity}]
  (let [entity-x (assoc entity :x (inc x))]
    (when (and me? (key-pressed? :enter) (u/get-touching-tile screen entity-x "graffiti"))
      (map-layer! (map-layer screen "graffiti") :set-visible true)
      (map-layer! (map-layer screen "hint") :set-visible false)))
  entity)

(defn animate
  [screen {:keys [x-velocity y-velocity
                  stand-right stand-left
                  jump-right jump-left
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
           (when (or (u/get-touching-tile screen entity-x "walls") (u/get-touching-tile screen entity-x "building"))
             {:x-velocity 0 :x-change 0 :x old-x})
           (when-let [tile (or (u/get-touching-tile screen entity-y "walls") (u/get-touching-tile screen entity-x "building"))]
             {:y-velocity 0 :y-change 0 :y old-y
              :can-jump? (not up?) :to-destroy false})
           (if (and (not me?) (u/get-touching-tile screen entity-x "turns"))
             (if (= direction :left)
               {:direction :right :x-velocity (* -1 x-velocity)}
               {:direction :left :x-velocity (* -1 x-velocity)})))));;(when up? tile)}))))
