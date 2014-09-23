(ns eu.cassiel.calamus.scene
  "Faux scene-graph drawing via quil"
  (:require [quil.core :as q]
            [quil.middleware :as m]))

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

(defn rect [& args]
  [:rect args])

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
  (doseq [[op arg children] nodes]
    (case op
      :rect (let [[cx cy w h] arg]
              (q/with-translation [cx cy 0] #_ (q/box w h 2) (q/rect 0 0 w h)))

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
      :layer (doseq [c arg] (render-nodes c)))))

(defn refresh
  "Refresh a scene, which contains the drawing elements as well as background,
   camera position, etc."
  [{:keys [background camera nodes]}]
  (when background (apply q/background background))

  (when-let [{:keys [position look-at up]} camera]
    (apply q/camera (flatten [position look-at up])))

  (render-nodes nodes)

  #_ (if-let [graphics (:graphics junk)]
    (do
      (q/with-graphics graphics
        (do (q/color-mode :rgb 1.0)
            (q/background 0 0 0 0)
            (q/hint :disable-depth-test)
            (q/rect-mode :center)
            (q/text-align :center :center)
            (q/ellipse-mode :radius)
            (render-nodes nodes junk)
            (when-let [blur (:blur junk)]
              (.set blur "blurSize" (int 11))
              (.set blur "sigma" (float 7.0))
              (q/filter-shader blur))))
      (q/image graphics 0 0))
    (render-nodes nodes junk)
    )

  #_
  (when-let [blur (:blur junk)]
    (.set blur "blurSize" (int 9))
    (.set blur "sigma" (float 5.0))
    (q/filter-shader blur))
  )
