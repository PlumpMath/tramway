(ns eu.cassiel.calamus.osc
  "Capture and format OSC for audio levels."
  (:require (eu.cassiel.monome-fu [network :as net])))

(defn listener [port]
  {:port port
   :rx (atom nil)
   :buffer (atom nil)})

(defn start [{:keys [port rx buffer]}]
  (swap! rx (fn [r]
              (when r (.close r))
              (net/start-receiver port
                                  (fn [src address args]
                                    (swap! buffer #(assoc %
                                                     (-> address
                                                         (clojure.string/replace "/" "")
                                                         (keyword))
                                                     (first args))))))))

(defn examine [{:keys [buffer]}]
  @buffer)

(defn stop [{:keys [rx buffer]}]
  (swap! rx #(when % (do (.close %) nil)))
  (reset! buffer nil))
