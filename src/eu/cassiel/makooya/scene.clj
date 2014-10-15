(ns eu.cassiel.makooya.scene
  "Faux scene-graph drawing via quil"
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defprotocol NODE
  (draw [this] "Draw (direct Quil calls).")
  (mouse [this click? x y] "Mouse position while mouse-down. `click?`=`true` when first down."))

(defn fill [colour & args]
  [:fill colour args])

(defn no-fill [& args]
  [:fill nil args])

(defn stroke [colour & args]
  [:stroke colour args])

(defn no-stroke [& args]
  [:stroke nil args])

(defn with-rotation [angle & args]
  [:with-rotation angle args])

(defn with-translation [xyz & args]
  [:with-translation xyz args])

(declare render-nodes)

(defn f_with-translation [xyz & args]
  (reify NODE
    (draw [this]
      (q/with-translation xyz (render-nodes args)))

    (mouse [this click? x y]
      (let [[dx dy] xyz]
        (doseq [n args] (mouse n click? (- x dx) (- y dy)))))))

(defn rect [& args]
  [:rect args])

(defn f_rect
  "New form: NODE instance."
  [cx cy z w h & {:keys [mouse-fn]}]
  (reify NODE
    (draw [this]
      (q/with-translation [cx cy z]
        (q/rect 0 0 w h)))

    (mouse [this click? x y]
      (when mouse-fn
        (let [x0 (- x cx)
              y0 (- y cy)]
          (if (and (< (Math/abs x0) (/ w 2))
                   (< (Math/abs y0) (/ h 2)))
            (mouse-fn :click? click? :x x0 :y y0)))))))


(defn tri-ptr [& args]
  [:tri-ptr args])

(defn disc [& args]
  [:disc args])

(defn text [& args]
  [:text args])

(defn layer [& args]
  [:layer args])

(defn render-nodes
  "Render the nodes, perhaps recursing for a 'scope' like `fill`, `stroke`,
   `translate`."
  [nodes]

  (doseq [n nodes]
    (if (vector? n)
      (let [[op arg children] n]
        (case op
          :rect (let [[cx cy w h] arg]
                  (q/with-translation [cx cy 0] #_ (q/box w h 2) (q/rect 0 0 w h)))

          ;; Triangle with base at (x1, y1), base width w, point at (x2, y2):
          :tri-ptr (let [[x1 y1 x2 y2 w] arg
                         w2 (* w 0.5)
                         xd (- x2 x1)
                         yd (- y2 y1)
                         len (Math/sqrt (+ (* xd xd) (* yd yd)))]
                     (q/with-translation [x1 y1 0]
                       (q/with-rotation #_ [0]
                         [(Math/atan2 (- y2 y1) (- x2 x1))]
                         (q/triangle 0 (+ w2)
                                     len 0
                                     0 (- w2)))))

          :disc (let [[cx cy r] arg]
                  (q/ellipse cx cy r r))

          :text (let [[p-font text x y w h] arg]
                  ;;(q/text-size size)
                  (q/text-font p-font)
                  (q/text text x y w h))

          :fill (let [curr-fill (q/current-fill)]
                  (if arg (apply q/fill arg) (q/no-fill))
                  (render-nodes children)
                  (q/fill curr-fill))

          :stroke (let [curr-stroke (q/current-stroke)]
                    (if arg (apply q/stroke arg) (q/no-stroke))
                    (render-nodes children)
                    (q/stroke curr-stroke))

          :with-translation (q/with-translation arg
                              (render-nodes children))

          :with-rotation (q/with-rotation
                           [(* arg q/TWO-PI)]
                           (render-nodes children))

          ;; :layer renders in order, so from back to front (we're not doing depth testing).
          :layer (doseq [c arg] (render-nodes c))))
      (draw n)
      )))

(defn refresh
  "Refresh a scene, which contains the drawing elements as well as background,
   camera position, etc."
  [{:keys [background camera nodes]} & {:keys [save-pattern]}]
  (when background (apply q/background background))

  ;;(println camera)
  (when-let [{:keys [position look-at up]} camera]
    (when (and position look-at up)
      (apply q/camera (flatten [position look-at up]))))

  (render-nodes nodes)

  (when save-pattern (q/save-frame save-pattern)))
