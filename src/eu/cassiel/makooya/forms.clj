(ns eu.cassiel.makooya.forms)

(defprotocol FORM
  "Specification for a graphical form."
  (init-struct-state [self]
    "Return an initial state for this form.")

  (automation-inits [self]
    "Return a map of initial values for Twizzle automation.")

  (automation-interps [self]
    "Return a map of interpolation functions for Twizzle automation.")

  (update [self form-state auto-state]
    "Update this state with automation state (partly vestigial - but
     lets us persist state beyond each `nodes` call). Can also alter
     automation state, so returns `{:form-state ..., :auto-state ...}`.")

  (nodes [self form-state auto-state]
    "Generate the drawing nodes for the form, given an automation state."))
