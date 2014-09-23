(ns eu.cassiel.calamus.auto
  (:require (eu.cassiel [calamus :as c])
            (eu.cassiel.calamus [queue :as queue])
            (eu.cassiel [twizzle :as tw])))

(def empty identity)

(defn auto [auto-fn & {:keys [ch at in to]
                       :or {at 0 in 0}}]
  (if (= to :same)
    auto-fn
    (comp #(-> %
               (tw/clear ch)
               (tw/automate-in ch at in to))
          auto-fn)))

(defn fire [auto-fn system]
  (queue/put (c/auto-queue system) auto-fn)
  empty)
