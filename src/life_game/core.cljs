(ns life-game.core
    (:require [reagent.core :as r :refer [atom]]))

(enable-console-print!)

;; globals
(def time-passes (atom false))

(def ticks (atom 0))

(def x (atom 15))
(def y (atom 15))
(def period (atom 100))
(def cell-size (atom 30))

(def cells (atom []))
(def cell-history (atom []))
(def match-str (atom " "))

(defn abs [n] (max n (- n)))

;; history
(defn reset-history []
  (reset! cell-history [])
  (reset! match-str " "))

(defn get-history [index]
  (get @cell-history index []))

(defn adv-history [old]
  (swap! cell-history conj old))

(defn check-history [v]
  (let [mid (.indexOf @cell-history v)]
    (if (>= mid 0)
      (swap! match-str str mid " ")))
  (some? (re-find #"(( \d+)+)\1" @match-str)))

;; Cell manipulation
(defn make-cell [state]
  {:alive state :initial state :to-live nil})

(defn generate-cells []
  (when-not @time-passes
    (reset-history)
    (reset! cells (vec (repeat @y (vec (repeat @x (make-cell false))))))
    (reset! ticks 0)))

(generate-cells);; initial call to make cell matrix

(defn check-cell [coll check-x check-y]
  (get-in coll [check-y check-x :alive]))

(defn get-neighbors [coll idx idy]
  (let [begin-x (if (= idx 0) idx (dec idx))
        end-x (if (= idx (dec @x)) (inc idx) (+ idx 2))
        begin-y (if (= idy 0) idy (dec idy))
        end-y (if (= idy (dec @y)) (inc idy) (+ idy 2))]
    (reduce
     +
     (map
      (fn [check-y]
        (count
         (filter
          true?
          (map (fn [check-x]
                 (when-not
                  (and
                   (= check-x idx)
                   (= check-y idy))
                   (check-cell coll check-x check-y)))
               (range begin-x end-x)))))
      (range begin-y end-y)))))

(defn should-live [coll idx idy]
  (let [live-neighbors (get-neighbors coll idx idy)
        current-life (check-cell coll idx idy)]
    (cond
      (and current-life (< live-neighbors 2)) false ;;Any live cell with fewer than two live neighbors dies, as if by under population.
      (and current-life (< live-neighbors 4)) true ;;Any live cell with two or three live neighbors lives on to the next generation.
      (and current-life (> live-neighbors 3)) false ;;Any live cell with more than three live neighbors dies, as if by overpopulation.
      (and (not current-life) (= live-neighbors 3)) true ;;Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
      :else current-life)))

(defn to-each-cell [f]
  (swap! cells (fn [coll]
                 (vec
                  (map-indexed
                   (fn [idy line]
                     (vec
                      (map-indexed
                       (fn [idx cell]
                         (f cell coll idx idy))
                       line)))
                   coll)))))

(defn generate-changes []
  (to-each-cell (fn [cell coll idx idy]
                  (assoc cell :to-live (should-live coll idx idy)))))

(defn apply-changes []
  (to-each-cell (fn [cell]
                  (when-not (nil? (:to-live cell))
                    (assoc cell :alive (:to-live cell))))))

(defn toggle-time-atom []
  (swap! time-passes not)
  (reset-history))

(defn tick []
  (swap! ticks inc)
  (adv-history @cells)
  (generate-changes)
  (apply-changes)
  (when (and
         @time-passes
         (check-history @cells))
    (toggle-time-atom)))

(defn untick []
  (swap! ticks dec)
  (swap! cells #(peek @cell-history))
  (swap! cell-history pop))

(defn time-on []
  (js/setTimeout (fn []
                   (tick)
                   (if @time-passes (time-on))) @period))

(defn toggle-time []
  (toggle-time-atom)
  (if @time-passes (time-on)))

(defn toggle-cell [idx idy]
  (if-not @time-passes
    (let [new-state (not (get-in @cells [idy idx :alive]))]
      (swap! cells update-in [idy idx] #(make-cell new-state)))))

(defn reset-dish []
  (when-not @time-passes
    (to-each-cell (fn [cell]
                    (assoc cell :alive (:initial cell))))
    (reset! ticks 0)
    (reset-history)))

(defn random-cell []
  (let [max-cell (* @x @y)
        target (rand-int max-cell)]
    [(quot target @y) (rem target @y)]))

(defn set-random []
  (generate-cells)
  (let [max-cell (* @x @y)
        times (abs (- (rand-int max-cell) (rand-int max-cell)))]
    (dotimes [n times]
      (apply toggle-cell (random-cell)))))


;; Display pieces
(defn time-passing []
  [:span (str @time-passes)])

(defn timer-component []
  [:div
   "Generations Elapsed: " @ticks])

(defn cell [idx model idy]
  [:div {:key idx :on-click #(toggle-cell idx idy) :style {:display "inline-block" :width (str @cell-size "px") :height (str @cell-size "px") :border "2px solid black" :background-color (if (:alive model) "white" "lightblue")}}])

(defn row [idy line]
  [:div {:key idy :style {:height (str @cell-size "px")}} (doall (map-indexed (fn [idx data] (cell idx data idy)) line))])

(defn dish []
  [:div (doall (map-indexed row @cells))])

(defn size-input []
  [:div
   [:div
    [:label "X:"]
    [:input {:type "number"
             :value @x
             :on-change #(reset! x (int (-> % .-target .-value)))}]
    [:label "Y:"]
    [:input {:type "number"
             :value @y
             :on-change #(reset! y (int (-> % .-target .-value)))}]]
   [:div
    [:label "Cell Size (px):"]
    [:input {:type "number"
             :value @cell-size
             :on-change #(reset! cell-size (int (-> % .-target .-value)))}]]
   [:div
    [:label "Delay (ms):"]
    [:input {:type "number"
             :value @period
             :on-change #(reset! period (int (-> % .-target .-value)))}]]])

(defn buttons []
  [:div
   [:input {:type "button"
            :value (if @time-passes "Stop" "Begin")
            :on-click toggle-time}]
   [:input {:type "button"
            :value "Tick"
            :on-click tick}]
   [:input {:type "button"
            :value "UnTick"
            :on-click untick}]
   [:input {:type "button"
            :value "Update Size"
            :on-click generate-cells}]
   [:input {:type "button"
            :value "Reset"
            :on-click reset-dish}]
   [:input {:type "button"
            :value "Random Seed"
            :on-click set-random}]])

(defn page []
  [:div
   [size-input]
   [buttons]
   [timer-component]
   [dish]])

(defn ^:export run []
  (r/render [page]
            (js/document.getElementById "app")))