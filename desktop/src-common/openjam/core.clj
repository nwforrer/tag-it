(ns openjam.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.ui :refer :all]
            [openjam.entities :as e]
            [openjam.utils :as u]))

(declare openjam-game title-screen main-screen text-screen)

(defn update-screen!
  [{:keys [current-map] :as screen} entities]
  (doseq [{:keys [x y height me?]} entities]
    (when me?
      (let [map-width (.get (.getProperties (.getMap current-map)) "width")
            half-width (/ (width screen) 2)
            new-x (if (< x half-width)
                    half-width
                    (if (> x (- map-width half-width))
                      (- map-width half-width)
                      x))]
        (position! screen new-x (/ u/vertical-tiles 2))
        (when (< y (- height))
          (set-screen! openjam-game main-screen text-screen)))))
  entities)

(defn detect!
  "Determines if the player is seen by a police officer. If so, game over."
  [screen entities]
  (if (:playing? screen)
    (let [player (first (filter :me? entities))
          police (remove :me? entities)]
      (doseq [{:keys [x y direction sight]} police]
        (when (and (= (int (:y player)) (int y))
                 (or (and (= direction :right) (> (:x player) x) (< (- (:x player) x) sight));;u/detect-distance))
                     (and (= direction :left) (< (:x player) x) (< (- x (:x player)) sight))));;u/detect-distance))))
          ;;(set-screen! openjam-game main-screen text-screen))))
          (sound! (:siren-sound screen) :play)
          (screen! text-screen :game-over)
          (update! screen :playing? false :lose? true)))))
  entities)

(defn spray!
  [screen {:keys [x me?] :as entity}]
  (let [entity-x (assoc entity :x (inc x))]
    (when (and me? (key-pressed? :enter) (u/get-touching-tile screen entity-x "graffiti"))
      (sound! (:spray-sound screen) :play)
      (map-layer! (map-layer screen "graffiti") :set-visible true)
      (map-layer! (map-layer screen "hint") :set-visible false)
      (update! screen :playing? false :win? true)
      (screen! text-screen :win)))
  entity)

(defn load-level!
  [screen level-name]
  (if-let [current-map (:current-map screen)]
    (tiled-map! screen :dispose))
  (let [current-map (orthogonal-tiled-map level-name (/ 1 u/pixels-per-tile))]
    (screen! text-screen :reset-labels)
    (update! screen :playing? true :win? false :lose? false :loaded? false :timeline [] :camera (orthographic) :renderer current-map :current-map current-map))
  [])

(defn load-entities
  [screen]
  (let [sheet (texture "Artist.png")
        police-sheet (texture "Police.png")
        tiles (texture! sheet :split 64 64)
        police-tiles (texture! police-sheet :split 64 64)
        player-images (for [col [0 1 2 3 4 5 6]]
                        (texture (aget tiles 0 col)))
        police-images (for [col [0 1 2 3 4]]
                        (texture (aget police-tiles 0 col)))
        player (apply e/create-player screen player-images)
        police (apply e/create-police screen police-images)]
    (update! screen :loaded? true)
    [player police]))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen
             :background-music (music "10 - The Empire.ogg" :set-looping true :play)
             :spray-sound (sound "spraycan.wav")
             :siren-sound (sound "siren.wav"))
      (load-level! screen @u/current-level))
  
  :on-render
  (fn [screen entities]
    (clear! 0.5 0.5 1 1)
    (if (not (:loaded? screen))
      (load-entities screen)
      (some->> (map (fn [entity]
                      (if (:playing? screen)
                        (->> entity
                             (e/move screen)
                             (e/prevent-move screen)
                             (e/animate screen)
                             (spray! screen))
                        entity))
                    entities)
               (detect! screen)
               (render! screen)
               (update-screen! screen))))

  :on-resume
  (fn [screen entities]
    entities)
  
  :on-resize
  (fn [{:keys [width height] :as screen} entities]
    (height! screen u/vertical-tiles))

  :on-hide
  (fn [screen entities]
    (music! (:background-music screen) :dispose))

  :on-key-down
  (fn [screen entities]
    (cond
      (= (:key screen) (key-code :r))
      (do
        (music! (:background-music screen) :dispose)
        (swap! u/current-level (fn [v] "level1.tmx"))
        (set-screen! openjam-game main-screen text-screen))
      (= (:key screen) (key-code :c))
      (do
        (music! (:background-music screen) :dispose)
        (set-screen! openjam-game main-screen text-screen))
      (= (:key screen) (key-code :escape))
      (set-screen! openjam-game title-screen)
      (= (:key screen) (key-code :p))
      (let [bg-music (:background-music screen)]
        (if (music! bg-music :is-playing)
          (music! bg-music :pause)
          (music! bg-music :play)))
      (= (:key screen) (key-code :enter))
      (if (:win? screen)
        (if-let [next-level (.get
                             (.getProperties
                              (.getMap (:current-map screen)))
                             "next-map")]
          (do (swap! u/current-level (fn [v] next-level))
              (set-screen! openjam-game main-screen text-screen))
           entities)))))

(defscreen text-screen
  :on-show
  (fn [screen entities]
    (update! screen :camera (orthographic) :renderer (stage))
    (assoc (label "0" (color :white))
           :id :fps
           :x 5))

  :game-over
  (fn [screen entities]
    (let [game-over-label
          (assoc (label "Game over" (color :white))
                 :id :game-over
                 :x 200
                 :y 200)]
      (conj entities game-over-label)))

  :win
  (fn [screen entities]
    (let [you-win-label
          (assoc (label "You've made your mark!" (color :white))
                 :id :you-win
                 :x 200
                 :y 200)
          continue-label (assoc (label "Press enter to continue..." (color :white))
                                :id :continue
                                :x 200
                                :y 150)]
      (conj entities you-win-label continue-label)))

  :reset-labels
  (fn [screen entities]
    [])
    
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

(defscreen title-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    (assoc (label "Press enter to start!" (color :white))
           :x 100
           :y 100))
    
  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))

  :on-key-down
  (fn [screen entities]
    (cond
      (= (:key screen) (key-code :enter))
      (set-screen! openjam-game main-screen text-screen))))

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
