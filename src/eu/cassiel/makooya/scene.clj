(ns eu.cassiel.makooya.scene
  "Faux scene-graph drawing via quil"
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defprotocol NODE
  (draw [this] "Draw (direct Quil calls).")
  (mouse [this click? x y] "Mouse position while mouse-down. `click?`=`true` when first down."))

(defn first-mouse-fn
  "Attempt mouse function call on sequence of nodes, exit when one returns truthy (has handled
   click). Note the `reverse` to take clicks from front to back."
  [nodes click? x y]
  (reduce (fn [state n] (or state (mouse n click? x y)))
          false
          (reverse nodes)))

(defn ^:deprecated OLD_fill [colour & args]
  [:fill colour args])

(defn fill [colour & args]
  (reify NODE
    (draw [this]
      (let [curr-fill (q/current-fill)]
        (if colour (apply q/fill colour) (q/no-fill))
        (doseq [n args] (draw n))
        (q/fill curr-fill)))

    (mouse [this click? x y]
      (first-mouse-fn args click? x y))))

(defn no-fill [& args]
  (apply fill nil args))

(defn ^:deprecated OLD_stroke [colour & args]
  [:stroke colour args])

(defn stroke [colour & args]
  (reify NODE
    (draw [this]
      (let [curr-stroke (q/current-stroke)]
        (if colour (apply q/stroke colour) (q/no-stroke))
        (doseq [n args] (draw n))
        (q/stroke curr-stroke)))

    (mouse [this click? x y]
      (first-mouse-fn args click? x y))))

(defn no-stroke [& args]
  (apply stroke nil args))

(defn ^:deprecated OLD_with-rotation [angle & args]
  [:with-rotation angle args])

(defn with-rotation [angle-xyz & args]
  (let [[angle x y z] (if (sequential? angle-xyz)
                        angle-xyz
                        [angle-xyz 0 0 1])
        rads (* angle q/TWO-PI)
        sin (Math/sin rads)
        cos (Math/cos rads)]
    (reify NODE
      (draw [this]
        (q/with-rotation
          [rads x y z]
          (doseq [n args] (draw n))))

      (mouse [this click? x y]
        (first-mouse-fn args click?
                        (+ (* x cos) (* y sin))
                        (- (* y cos) (* x sin)))))))

(defn ^:deprecated OLD_with-translation [xyz & args]
  [:with-translation xyz args])

(declare render-nodes)

(defn with-translation [xyz & args]
  (reify NODE
    (draw [this]
      (q/with-translation xyz (render-nodes args)))

    (mouse [this click? x y]
      (let [[dx dy] xyz]
        (first-mouse-fn args click? (- x dx) (- y dy))))))

(defn ^:deprecated OLD_rect [& args]
  [:rect args])

(defn rect
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
          (when (and (< (Math/abs x0) (/ w 2))
                     (< (Math/abs y0) (/ h 2)))
            (mouse-fn :click? click? :x x0 :y y0)))))))

(defn tri-ptr [& args]
  [:tri-ptr args])

(defn ^:deprecated OLD_disc [& args]
  [:disc args])

(defn disc [cx cy z r]
  (reify NODE
    (draw [this]
      (q/with-translation [cx cy z]
        (q/ellipse 0 0 r r)))

    (mouse [this click? x y]) ;; TODO
    )
  )

(defn text [& args]
  [:text args])

(defn ^:deprecated OLD_layer [& args]
  [:layer args])

(defn layer [& layers]
  (reify NODE
    (draw [this]
      (doseq [a layers
              n a]
        (draw n)))

    (mouse [this click? x y]
      ;; Here we *do* reverse the layers, to reflect back-to-front rendering.
      (reduce (fn [state ns] (or state (first-mouse-fn ns click? x y)))
              false
              (reverse layers)))))

(defn ^:deprecated render-nodes
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
