(ns eu.cassiel.calamus
  (:require (eu.cassiel.calamus [scene :as scene]
                                [queue :as queue]
                                [forms :as f])
            (eu.cassiel [twizzle :as tw])
            (eu.cassiel.twizzle [interpolators :as twi])
            (tween-clj [core :as tween])
            [quil.core :as q]
            [quil.middleware :as qm]))

(defprotocol APP
  "Rather gratuitous protocol for the main application."
  (start [this] "Start the application.")
  (stop [this] "Stop the application.")
  (auto-queue [this] "Returns a reference to the automation queue."))

(defn jump-interp
  "An interpolator for Twizzle which jumps to its final value at the end (and returns
   its initial value before that)."
  [val-1 _ _]
  val-1
  )

(defn create-app [forms]
  (let [auto-Q (queue/queue)
        sketch (atom nil)

        windowed-config {:size [800 600]}
        macbook-config {:size :fullscreen :features [:present] :display 0}
        mac-pro-config {:size :fullscreen :features [:present] :display 1}

        config macbook-config

        stop' (fn []
                (swap! sketch #(do (when % (-> % (.frame) (.dispose)))
                                   nil)))

        initial-state
        (let [global-inits {[:scene :bg] [0 0 0]
                            [:camera :position] [0 0 300]
                            [:camera :look-at]  [0 0 0]
                            [:camera :up] [0 1 0]}
              expo (twi/wrap-tween tween/ease-in-out tween/transition-expo)
              expo-vec (partial twi/interp-vectors expo)
              global-interps {[:scene :bg] twi/interp-vectors
                              [:camera :position] expo-vec
                              [:camera :look-at] expo-vec
                              [:camera :up] expo-vec}
              all-inits (reduce merge global-inits (map f/automation-inits forms))
              all-interps (reduce merge global-interps (map f/automation-interps forms))
              ;; Pair forms with their states.
              forms (map (fn [s] {:form s
                                 :state (f/init-struct-state s)})
                         forms)]
          ;; :scene gets created by automation, so the renderer is nil-protected (perhaps
          ;; the first drawing happens before any update?)
          {:forms forms
           :automation {:state (tw/initial
                                :init all-inits
                                :interp all-interps)
                        :queue auto-Q}})

        setup (fn []
                (q/color-mode :rgb 1.0)
                (q/hint :disable-depth-test)
                (q/rect-mode :center)
                (q/text-align :center :center)
                (q/ellipse-mode :radius)
                (q/frame-rate 30)
                initial-state)

        update (fn [state]
                 (let [auto-fn (let [q (get-in state [:automation :queue])]
                                 (queue/take q))
                       automation' ((or auto-fn identity)
                                    (get-in state [:automation :state]))
                       automation'' (tw/locate automation' (/ (q/millis) 1000))
                       bg (tw/sample automation'' [:scene :bg])
                       camera {:position (tw/sample automation'' [:camera :position])
                               :look-at (tw/sample automation'' [:camera :look-at])
                               :up (tw/sample automation'' [:camera :up])}]
                   (as-> state S
                         (update-in S [:forms]
                                    (partial map (fn [{:keys [form state]}]
                                                   {:form form
                                                    :state (f/update form
                                                                     state
                                                                     automation'')})))
                         (assoc-in S [:scene :nodes]
                                   [[:layer (map (fn [{:keys [form state]}]
                                                   (f/nodes form state automation''))
                                                 (:forms S))]])
                         (assoc-in S [:automation :state] automation'')
                         (assoc-in S [:scene :background] bg)
                         (assoc-in S [:scene :camera] camera))))

        draw (fn [{scene :scene}]
               (scene/refresh scene))

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
        this)

      (auto-queue [this] auto-Q))))
