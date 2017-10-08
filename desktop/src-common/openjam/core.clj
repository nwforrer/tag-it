(ns openjam.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.ui :refer :all]
            [openjam.entities :as e]
            [openjam.utils :as u]))

(declare openjam-game main-screen text-screen)

(defn update-screen!
  [screen entities]
  (doseq [{:keys [x y height me? to-destroy]} entities]
    (when me?
      (position! screen x (/ u/vertical-tiles 2))
      (when (< y (- height))
        (set-screen! openjam-game main-screen text-screen)))
    (when-let [[tile-x tile-y] to-destroy]
      (tiled-map-layer! (tiled-map-layer screen "walls")
                        :set-cell tile-x tile-y nil)))
  (map #(dissoc % :to-destroy) entities))

(defn detect!
  "Determines if the player is seen by a police officer. If so, game over."
  [screen entities]
  (let [player (first (filter :me? entities))
        police (remove :me? entities)]
    (doseq [{:keys [x y direction]} police]
      (if (and (= (int (:y player)) (int y))
               (or (and (= direction :right) (> (:x player) x))
                   (and (= direction :left) (< (:x player) x))))
        (set-screen! openjam-game main-screen text-screen))))
  entities)

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :background-music (music "01 - Opening.ogg" :set-looping true))
    (->> (orthogonal-tiled-map "level1.tmx" (/ 1 u/pixels-per-tile))
         (update! screen :timeline [] :camera (orthographic) :renderer))
    (let [sheet (texture "Artist.png")
          police-sheet (texture "Police.png")
          tiles (texture! sheet :split 64 64)
          police-tiles (texture! police-sheet :split 64 64)
          player-images (for [col [0 1 2 3 4]]
                          (texture (aget tiles 0 col)))
          police-images (for [col [0 1 2 3 4]]
                          (texture (aget police-tiles 0 col)))
          player (apply e/create-player player-images)
          police (apply e/create-police police-images)]
      [player police]))
  
  :on-render
  (fn [screen entities]
    (clear! 0.5 0.5 1 1)
    (some->> (if (or (key-pressed? :space) (u/touched? :center))
               (rewind! screen 2)
               (map (fn [entity]
                      (->> entity
                           (e/move screen)
                           (e/prevent-move screen)
                           (e/animate screen)
                           (e/spray screen)))
                    entities))
             (detect! screen)
             (render! screen)
             (update-screen! screen)))
  
  :on-resize
  (fn [{:keys [width height] :as screen} entities]
    (height! screen u/vertical-tiles))

  :on-key-down
  (fn [screen entities]
    (cond
      (= (:key screen) (key-code :r))
      (set-screen! openjam-game main-screen text-screen)
      (= (:key screen) (key-code :p))
      (let [bg-music (:background-music screen)]
        (if (music! bg-music :is-playing)
          (music! bg-music :pause)
          (music! bg-music :play))))))

(defscreen text-screen
  :on-show
  (fn [screen entities]
    (update! screen :camera (orthographic) :renderer (stage))
    (assoc (label "0" (color :white))
           :id :fps
           :x 5))
  
  :on-render
  (fn [screen entities]
    (->> (for [entity entities]
           (case (:id entity)
             :fps (doto entity (label! :set-text (str (game :fps))))
             entity))
         (render! screen)))
  
  :on-resize
  (fn [screen entities]
    (height! screen 300)))

(defscreen blank-screen
  :on-render
  (fn [screen entities]
    (clear!))

  :on-key-down
  (fn [screen entities]
    (set-screen! openjam-game main-screen text-screen)))

(set-screen-wrapper! (fn [screen screen-fn]
                       (try (screen-fn)
                            (catch Exception e
                              (.printStackTrace e)
                              (set-screen! openjam-game blank-screen)))))

(defgame openjam-game
  :on-create
  (fn [this]
    (set-screen! this main-screen text-screen)))
