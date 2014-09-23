(ns eu.cassiel.calamus.queue
  "Simple non-blocking queues.")

(defn queue
  "Create a queue."
  []
  (atom {:q nil :seen nil}))

(defn put
  "Add an item to a queue."
  [queue x]
  (swap! queue update-in [:q] #(conj % x))
  queue)

(defn take
  "Take an item from a queue atomically, return nil if the queue is empty.
   Linear time-complexity, so keep it short."
  [queue]
  (:seen (swap!
          queue
          (fn [queue]
            (if-let [q (seq (:q queue))]
              (assoc queue :seen (last q) :q (butlast q))
              (dissoc queue :seen))))))
