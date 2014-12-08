(ns automata.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn seed-row [hw]
  "hw = number of 0s to pad on each side."
  (concat (repeat hw 0) (conj (repeat hw 0) 1)))

(assert (= (seed-row 3) '(0 0 0 1 0 0 0)))



;(defn neighbors [x]
;  "Get's immediate neighbors on the x-axis"
;  (for [dx [-1 1]]
;    (+ dx x)))

;(defn cell+neighbors [x]
;  (for [dx [-1 0 1]]
;    (+ dx x)))


(defn generate-next [row rule]
  (let [g (partition 3 1 row)]
    ;(println "next " rule vec g (map vec g) (map rule (map vec g)))
    (concat (concat [0] (map rule (map vec g))) [0])))

(def rule30
  "Not sure if this is idiomatic in Clojure, but everything's a function, right?"
  {
    [0 0 0] 0
    [0 0 1] 1
    [0 1 0] 1
    [0 1 1] 1
    [1 0 0] 1
    [1 0 1] 0
    [1 1 0] 0
    [1 1 1] 0})


(assert
  (=
    (generate-next (seed-row 5) rule30)
    '(0 0 0 0 1 1 1 0 0 0 0)))

(assert
  (=
    (generate-next (vec '(0 0 1 0 0 1 0 0 1)) rule30)
    '(0 1 1 1 1 1 1 1 0)))


;(defn mouse-clicked [state event]
;  "Mouse handler to reset state"
;  (q/background 240)
;  (assoc state :row 0 :cells [(first-row)]))


(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 5)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state: row index and current cells
  {
    :row 0
    :cells [(seed-row 20)]})  ; y (row) implicit as index, x (cols) as value

(defn update [state]
  "Calculates next row in state"
  (let [cells (:cells state)
        next-row (generate-next (last cells) rule30)]
  (assoc state
    :cells (conj cells (vec next-row))
    :row (inc (:row state)))))

(defn draw [state]
  "Draws current state"

  ;(q/background 240) ; background color
  (q/stroke 220) ; cell border color
  (q/fill 0) ; cell body color

  (doseq [x (map-indexed vector (last (:cells state)))
          :let [y (:row state)
                w 20]]
    (if (= 1 (last x))
      (q/rect (* w (first x)) (* w y) w w))))

(q/defsketch automata
  :title "You spin my circle right round"
  :size (vec (take 2 (repeat (* 20 100))))
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update is called on each iteration before draw is called.
  ; It updates sketch state.
  :update update
  :draw draw
  :mouse-clicked mouse-clicked
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
