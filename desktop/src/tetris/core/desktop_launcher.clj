(ns tetris.core.desktop-launcher
  (:require [tetris.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. tetris-game "tetris" 900 1200)
  (Keyboard/enableRepeatEvents true))
