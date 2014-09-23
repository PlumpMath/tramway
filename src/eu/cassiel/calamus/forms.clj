(ns eu.cassiel.calamus.forms)

(defprotocol FORM
  "Specification for a graphical form."
  (init-struct-state [self]
    "Return an initial state for this form.")

  (automation-inits [self]
    "Return a map of initial values for Twizzle automation.")

  (automation-interps [self]
    "Return a map of interpolation functions for Twizzle automation.")

  (update [self struct-state auto-state levels]
    "Update this state with a map of instantaneous levels (probably called `:A` upwards).
     Useful for doing something like keeping a time-based history. Return a new state.")

  (nodes [self struct-state auto-state]
    "Generate the drawing nodes for the form, given an automation state."))
