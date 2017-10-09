(ns openjam.core.desktop-launcher
  (:require [openjam.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. openjam-game "Tag It" 800 600)
  (Keyboard/enableRepeatEvents true))
