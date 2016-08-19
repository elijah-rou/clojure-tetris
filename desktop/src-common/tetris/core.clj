(ns tetris.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.math :refer :all]
            [clojure.math.combinatorics :as combo]))

(def move-speed 60) ; Move pixels by this much
(def initial-state (atom 0)) ; 0-Start, 1-In play, 2-End
(def score (atom 0)) ; Game Score
(def counter (atom 1)) ; Block-State index number
(def current-state (atom 1)) ; Game states of blocks
(declare main-screen open-screen tetris-game lose-screen get-row block-pop)
(def state-map ; Possible Game states of blocks in a map
  (atom
    {:1 1 ;state 1
     :2 2 ;state 2
     :3 3 ;state 3
     :4 4 ;state 4
     :5 5 ;state 5
     :6 6 ;state 6
     :7 7})) ; state 7
     
(defn- change-state []
  (swap! counter inc)
  (reset! current-state
    (case (deref counter)
      2 (:2 (deref state-map))
      3 (:3 (deref state-map))
      4 (:4 (deref state-map))
      5 (:5 (deref state-map))
      6 (:6 (deref state-map))
      7 (:7 (deref state-map))
      8 "INVALID")))

(defn- random-array []
  "Generate a random array of unique numbers from 1-7"
  (shuffle [1 2 3 4 5 6 7]))

(defn- reset-state []
  "Reset game state with new state-map"
  (let [array (random-array)]
    (reset! counter 1)
    (reset! state-map {
                       :1 (nth array 0)
                       :2 (nth array 1)
                       :3 (nth array 2)
                       :4 (nth array 3)
                       :5 (nth array 4)
                       :6 (nth array 5)
                       :7 (nth array 6)})
    (reset! current-state (:1 (deref state-map)))))

(defn texture-encode[num]
  "Map numbers to textures"
  (case num
    1 "puzzleGraphics/png/element_red_rectangle_glossy.png"
    2 "puzzleGraphics/png/element_grey_rectangle_glossy.png"
    3 "puzzleGraphics/png/element_green_rectangle_glossy.png"
    4 "puzzleGraphics/png/element_blue_rectangle_glossy.png"
    5 "puzzleGraphics/png/element_purple_rectangle_glossy.png"
    6 "puzzleGraphics/png/element_yellow_rectangle_glossy.png"
    7 "puzzleGraphics/png/element_blue_rectangle_glossy.png"))

(defn block-encode[num]
  "Map numbers to blocks"
  (case num
    1 "stick"
    2 "l-block"
    3 "j-block"
    4 "z-block"
    5 "cube"
    6 "t-block"
    7 "s-block"))

(defn- block-generator []
  "Generate array fo blocks in random order"
  (map block-encode (random-array)))

;; map forming functions

(defn- build-block [array]
  "Convert a block matrix to pixel form"
  (for [i array]
    (map #(+ 5 (* 60 %)) i)))

(defn- co-ord-block [array]
  "Generate array of x-y maps representing co-ords"
  (for [i array]
    (array-map :x (first i) :y (last i))))

(defn- block-array []
  "Generate an array of pixel/map transformed blocks"
  (map co-ord-block (map build-block block-generator)))
  
(defn- direction? []
  "Obtain direction for key-press"
  (cond
    (key-pressed? :dpad-left) :left
    (key-pressed? :a) :left
    (key-pressed? :dpad-right) :right
    (key-pressed? :d) :right
    (key-pressed? :dpad-down) :down
    (key-pressed? :s) :down))

(defn- column-to-x [col-num]
  "Convert a column number to x value"
  (+ (* move-speed (dec col-num)) 5))

(defn- row-to-y [row-num]
  "Convert a row number to y value"
  (- 1205 (* move-speed row-num)))
    
(defn- has-coord [{:keys [block?] :as entity} column row]
  "Check is a block has particular co-ordinates"
  (if block?
    (if (and (= (:column entity) column) (= (:row entity) row) (not= (:game-state entity) 0))
      true 
      false)))
    
(defn- is-occupied? [entities column row]
  "Is a certain column/row combo in virtual matrix taken? If True return true, if false nil"
  (some true? (map #(has-coord % column row) (filter #(= (:game-state %) 2) entities))))

(defn- move-start [{:keys [block? game-state] :as entity}]
  ;;(println game-state)
  (if (and block? (= game-state 0))
    (let [start-x (column-to-x (:column entity))
          start-y (row-to-y (:row entity))]
      ;(println "Move to starting position")
      ;(println start-x)
      ;(println start-y)
      (assoc entity :x start-x :y start-y :game-state 1))
    entity))

(defn- change-game-state [{:keys [block? game-state] :as entity}]
  (if (and block? (= game-state 1))
    (assoc entity :game-state 2)
    entity))

(defn- stop-blocks [entities]
  (map change-game-state entities))
    ;;(->> entities
      ;;   (map change-game-state))
       
(defn- update-block-position [entities {:keys [block? game-state] :as entity}]
  "Give block new position on virtual matrix and screen from input"
  (if (and block? (= game-state 1))
    (let [direction (direction?)
          new-pos (case direction
                        :right (+ (:x entity) move-speed)
                        :left (- (:x entity) move-speed)
                        :down (- (:y entity) move-speed))
          new-matrix-pos (case direction
                            :right (inc (:column entity))
                            :left (dec (:column entity))
                            :down (inc (:row entity)))]                             
        (case direction
          :right (if (> new-pos 545)
                   false
                   (if (is-occupied? entities new-matrix-pos (:row entity))
                     false
                     (assoc entity :x new-pos :column new-matrix-pos)))
          :left (if (< new-pos 5)
                  false
                  (if (is-occupied? entities new-matrix-pos (:row entity))
                    false
                    (assoc entity :x new-pos :column new-matrix-pos)))             
          :down (if (< new-pos 5) ;; down don't work
                  true
                  (if (is-occupied? entities (:column entity) new-matrix-pos)
                    true
                    (assoc entity :y new-pos :row new-matrix-pos)))))           
    entity))

(defn- move-down [entities {:keys [block? game-state] :as entity}]
  "Passively move block down on virtual matrix and screen"
    (if (and block? (= game-state 1))
      (let [new-y (- (:y entity) move-speed)
            new-row (inc (:row entity))]       
        (if (< new-y 5)
          ;;(println "a block got to the end")
          true
          ;;(assoc entity :y 5 :game-state 2)
          (if (is-occupied? entities (:column entity) new-row)
            true ;;(assoc entity :game-state 2) 
            (assoc entity :y new-y :row new-row))))   
      entity))

(defn- spawn-origin-block [num column row]
  "Associate a block"
  (assoc (texture (texture-encode num))
    :column column
    :x (+ (column-to-x column) 450) 
    :row row
    :y (- (row-to-y row) 180)
    :width 54 
    :height 54 
    :block? true 
    :type (block-encode num)
    :game-state 0 ; 0-Displayed, 1-In motion, 2-Placed
    :origin true))
  
  
(defn- spawn-block [num column row]
  "Associate a block"
  (assoc (texture (texture-encode num))
    :column column
    :x (+ (column-to-x column) 450) 
    :row row
    :y (- (row-to-y row) 180)
    :width 54 
    :height 54 
    :block? true 
    :type (block-encode num)
    :game-state 0 ; 0-Displayed, 1-In motion, 2-Placed
    :origin false)) 
  
(defn- spawn [entities state]
  "Spawn a series of blocks to form tetrimino"
    (case state
      ;; Spawn Stick
      1 (do
          (-> entities
            (conj (spawn-block state 5 4))
            (conj (spawn-origin-block state 5 3))
            (conj (spawn-block state 5 2))
            (conj (spawn-block state 5 1))
            (vec)))   
      ;; Spawn l-block
      2 (do
          (-> entities
            (conj (spawn-origin-block state 5 3))
            (conj (spawn-block state 6 3))
            (conj (spawn-block state 5 2))
            (conj (spawn-block state 5 1))
            (vec)))
      ;; Spawn j-block  
      3 (do
          (-> entities
            (conj (spawn-block state 5 3))
            (conj (spawn-origin-block state 6 3))
            (conj (spawn-block state 6 2))
            (conj (spawn-block state 6 1))
            (vec)))
      ;; Spawn z-block
      4 (do
          (-> entities
            (conj (spawn-origin-block state 6 2))
            (conj (spawn-block state 7 2))
            (conj (spawn-block state 5 1))
            (conj (spawn-block state 6 1))
            (vec)))       
      ;; Spawn cube
      5 (do
          (-> entities
            (conj (spawn-origin-block state 5 2))
            (conj (spawn-block state 6 2))
            (conj (spawn-block state 5 1))
            (conj (spawn-block state 6 1))
            (vec)))          
      ;; Spawn t-block
      6 (do
          (-> entities
            (conj (spawn-block state 5 3))
            (conj (spawn-origin-block state 5 2))
            (conj (spawn-block state 6 2))
            (conj (spawn-block state 5 1))
            (vec)))         
      ;; Spawn s-block
      7 (do
          (-> entities
            (conj (spawn-origin-block state 6 2))
            (conj (spawn-block state 5 2))
            (conj (spawn-block state 6 1))
            (conj (spawn-block state 7 1))))))
              
       
(defn- spawn-check [entities]
  "Return false if there are moveable block entities on the map"
  (if (>= (reduce + (map #(if (= (:game-state %) 1) 1 0) entities)))
    false
    true))

(defn- get-row[entities row-num]
  (filter #(= (:row %) row-num) entities))

(defn- on-the-row? [{:keys [block? state] :as entity} row-num]
  (if (and block? (not= 0 state))
    (if (>= row-num (:row entity))
      (let [new-row (inc (:row entity))]
        (assoc entity :row new-row :y (row-to-y new-row))) 
      entity)
    entity))

(defn- add-score [{:keys [score?] :as entity}]
  (if score? 
    (label! entity :set-text (str "Current Score: " (str (deref score))))
    entity))

(defn- block-pop-supp [entities row-num]
  "Pop a row of blocks if row is full"
  (let [ent-row (get-row entities row-num)]
    (if (= (count ent-row) 10)
      (let [new-ent (map #(on-the-row? % (- row-num 1)) (remove (set ent-row) entities))]
        (swap! score inc)
        (println (str "Current Score: " (str (deref score))))
        (map add-score new-ent))
      entities))) 

(defn- block-pop [entities]
  "Run through all rows and pop"
  (-> entities
      (block-pop-supp 1)
      (block-pop-supp 2)
      (block-pop-supp 3)
      (block-pop-supp 4)
      (block-pop-supp 5)
      (block-pop-supp 6)
      (block-pop-supp 7)
      (block-pop-supp 8)
      (block-pop-supp 9)
      (block-pop-supp 10)
      (block-pop-supp 11)
      (block-pop-supp 12)
      (block-pop-supp 13)
      (block-pop-supp 14)
      (block-pop-supp 15)
      (block-pop-supp 16)
      (block-pop-supp 17)
      (block-pop-supp 18)
      (block-pop-supp 19)
      (block-pop-supp 20)))
  
(defn- move-to-start [entities]
  (map move-start entities))
 
(defn- move-block [entities]
  "Actively move state-1 blocks with input. If spawn ready, change state and spawn block"
  (let [new-ent (map #(update-block-position entities %) entities)]
    (if (some true? new-ent)
      (let [stopped-ent (block-pop (stop-blocks entities))
            new-ent2 (move-to-start stopped-ent)]
        (change-state)
        (if (= 8 (deref counter))
          (reset-state))
        (spawn new-ent2 (deref current-state)))
      (if (some false? new-ent)
        entities
        new-ent))))
      
(defn- gravitate-block [entities]
  "Passively move state-1 blocks down. If spawn ready, change state and spawn block"
  (if (= (deref initial-state) 0)
    (do 
      (let [new-ent (move-to-start entities)]
        (change-state)
        (swap! initial-state inc)
        (spawn new-ent (deref current-state))))
    (do
      (let [new-ent (map #(move-down entities %) entities)]
        (if (some true? new-ent)
          (let [stopped-ent (block-pop (stop-blocks entities))
                new-ent2 (move-to-start stopped-ent)]
               (change-state)
               (if (= 8 (deref counter))
                 (reset-state))
               ;(println "Reset and spawn")
               (spawn new-ent2 (deref current-state)))
          new-ent)))))


(defn- rotate [entities {:keys [block? origin game-state] :as entity} orig-x orig-y]
  "Apply linear transform of -90"
  (if (and block? (= game-state 1))
      (let [x (- (:column entity) orig-x)
            y (- (:row entity) orig-y)
            new-x (+ y orig-x)
            new-y (+ (* -1 x) orig-y)]
        (if (or (is-occupied? entities new-x new-y) (> new-y 20) (> new-x 10) (< new-x 0))
          true
          (assoc entity :column new-x :row new-y :x (column-to-x new-x) :y (row-to-y new-y))))
    entity))

(defn- trigger-rotate [entities]
  "Find the moving blocks and perform rotation"
  (let [orig-ent (first (filter #(and (true? (:origin %)) (= (:game-state %) 1)) entities))
        origin-x (:column orig-ent)
        origin-y (:row orig-ent)
        new-ent (map #(rotate entities % origin-x origin-y) entities)]
    (if (some true? new-ent)
      entities
      new-ent)))    

(defscreen main-screen
  
  :on-show
  (fn [screen entities]
    (reset! initial-state 0)
    (reset-state)
    (update! screen :renderer (stage))
    (update! screen :timeline [])
    (add-timer! screen :gravitate-block 0  0.5)
    (let [background (assoc (texture "Grid3New2.png") :x 605 :width 5 :height 1200 :background? true)
          next-label (assoc (label "Next Block" (style :label (bitmap-font) (color :white)) 
                              :set-font-scale-x 3 :set-font-scale-y 3) :x 650 :y 1100)
          score-label (assoc (label "Current Score: 0" (style :label (bitmap-font) (color :white)) 
                               :set-font-scale-x 2 :set-font-scale-y 2) :x 650 :y 500 :score? true)
          spawn (spawn entities (deref current-state))]
      [background next-label score-label spawn]))
          
  :on-render
  (fn [screen entities]
    (clear!)
    (if (not= (deref initial-state) 2)
      (->> entities
         (render! screen))
      (set-screen! tetris-game lose-screen)))
   
  
  :on-key-down
  (fn [screen entities]
    (cond
      ;(key-pressed? :r) (app! :post-runnable #(set-screen! tetris-game main-screen))
      (key-pressed? :p) (println entities)
      (key-pressed? :t) (rewind! screen 50)
      (key-pressed? :dpad-up) (trigger-rotate entities)
      (key-pressed? :w) (trigger-rotate entities)
      (direction?) (move-block entities)))

  
  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :gravitate-block (gravitate-block entities))))
  
(defscreen open-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage)))

  :on-render
  (fn [screen entities]
    (clear!)
    (->> entities
         (render! screen)))

  :on-touch-down
  (fn [screen entities]
    (set-screen! tetris-game main-screen)))

(defscreen lose-screen
  
  :on-show
  (fn [screen entities]
    (reset! initial-state 0)
    (println "You lose")
    (-> main-screen :entities deref)
    (update! screen :renderer (stage))
    [])
      
  :on-render
  (fn [screen entities]
    (clear!)
    (->> entities
         (render! screen)))

  :on-touch-down
  (fn [screen entities]
    (set-screen! tetris-game main-screen)))

(defgame tetris-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))

(-> main-screen :entities deref)

