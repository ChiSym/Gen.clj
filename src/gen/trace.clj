(ns gen.trace
  "Protocols that constitute the trace interface.

  See [the
  docs](https://github.com/probcomp/Gen.jl/blob/master/src/gen_fn_interface.jl#L1)
  for more detail."
  (:refer-clojure :exclude [update]))

(defprotocol GenFn
  :extend-via-metadata true
  (gf [trace]
    "Returns the generative function that produced the given trace."))

(defprotocol Args
  :extend-via-metadata true
  (args [trace]
    "Returns the argument tuple for a given execution."))

(defprotocol RetVal
  :extend-via-metadata true
  (retval [trace]
    "Returns the return value of the given execution."))

(defprotocol Choices
  :extend-via-metadata true
  (choices [trace]
    "Returns a value implementing the assignment interface."))

(defprotocol Score
  :extend-via-metadata true
  (score [trace]))

(defprotocol Update
  (update
    [trace constraints]
    [trace args argdiffs constraints]
    "Updates a trace by changing the arguments and / or providing new values for
    some existing random choice(s) and values for some newly introduced random
    choice(s).

    The two-arity version is a shorthand variant of `gen.trace/update` which
    assumes the arguments are unchanged.

    TODO deal with this implementation somewhere?"))

#_
(defprotocol Project
  :extend-via-metadata true
  (project [trace selection]
    "Estimates the probability that the selected choices take the values they do
    in a trace.")
  )
#_
(defprotocol Regenerate
  (regenerate
    [trace args argdiffs selection]
    "Updates a trace by changing the arguments and / or randomly sampling new
    values for selected random choices using the internal proposal distribution
    family."

    [trace selection]
    "Shorthand variant of regenerate which assumes the arguments are
    unchanged."))
