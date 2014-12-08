(ns automata.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def constants {:scale 5})

(defn make-row [{:keys [n around middle scale]}]
  (let [c (vec (take (if scale (* scale n) n) (repeat around)))]
  (assoc c (/ (count c) 2) middle)))

(def first-row
  #(make-row {:n 20 :around 0 :middle 1 :scale (constants :scale)}))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 5)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:scale (constants :scale)
   :rule {
          [0 0 0] 0
          [0 0 1] 1
          [0 1 0] 1
          [0 1 1] 1
          [1 0 0] 1
          [1 0 1] 0
          [1 1 0] 0
          [1 1 1] 0} ; rule 30
   :row 0
   :cells [(first-row)]; y (row) implicit as index, x (cols) as value
   })

(defn neighbors [x]
  "Get's immediate neighbors on the x-axis"
  (for [dx [-1 1]]
  (+ dx x)))

(defn cell+neighbors [x]
  (for [dx [-1 0 1]]
  (+ dx x)))

(defn generate-next [row rule]
  (let [g (partition 3 1 row)]
    (println "next " (map rule (map vec g)))
    (map rule (map vec g)))
  )


(defn update [state]
  "Calculates next row in state"
  (let [cells (:cells state)
        rule (:rule state)
        next-row (generate-next (last cells) rule)]
;;     (println "updating" (state :row))
;;     (println "cells" (state :cells))

    (assoc state :cells (conj cells (vec next-row)) :row (inc (:row state)))))

(defn draw [state]
  "Draws current state"

  ;(q/background 240) ; background color
  (q/stroke 220) ; cell border color
  (q/fill 0) ; cell body color

  (doseq [x (map-indexed vector (last (:cells state)))
          :let [y (:row state)
                w (:scale state)]]
    (if (= 1 (last x))
      (q/rect (* w (first x)) (* w y) w w)))

  ;(if (<= (:frame state) 117) (q/save-frame "frame-####.png"))
  )

(defn mouse-clicked [state event]
  "Mouse handler to reset state"
  (q/background 240)
  (assoc state :row 0 :cells [(first-row)]))

(q/defsketch automata
  :title "You spin my circle right round"
  :size (vec (take 2 (repeat (* (constants :scale) 100))))
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
