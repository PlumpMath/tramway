(ns eu.cassiel.calamus
  (:require [quil.core :as q]
            [quil.middleware :as qm]))

(defprotocol APP
  "Rather gratuitous protocol for the main application."
  (start [this] "Start the application.")
  (stop [this] "Stop the application."))

(defn create-app []
  (let [sketch (atom nil)

        windowed-config {:size [800 600]}
        macbook-config {:size :fullscreen :features [:present] :display 0}
        mac-pro-config {:size :fullscreen :features [:present] :display 1}

        config windowed-config

        stop' (fn []
                (swap! sketch #(do (when % (-> % (.frame) (.dispose)))
                                   nil)))

        setup (fn []
                (q/color-mode :rgb 1.0)
                (q/hint :disable-depth-test)
                (q/rect-mode :center)
                (q/text-align :center :center)
                (q/ellipse-mode :radius)
                (q/frame-rate 30)
                nil)

        update (fn [state] state)

        draw (fn [state] (q/background 0.2))

        start' (fn []
                 (stop')
                 (reset! sketch
                         (q/sketch ;; :features [:no-safe-fns]
                          :size (:size config)
                          :features (:features config)
                          :display (:display config)
                          :renderer :p3d
                          :setup setup
                          :update update
                          :draw draw
                          :middleware [qm/pause-on-error qm/fun-mode])))]

    (reify APP
      (start [this]
        (start')
        this)

      (stop [this]
        (stop')
        this))))
