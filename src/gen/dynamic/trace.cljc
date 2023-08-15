(ns gen.dynamic.trace
  #?(:clj (:refer-clojure :exclude [promise]))
  (:require [gen.dynamic.choice-map :as dynamic.choice-map]
            [gen.trace :as trace])
  #?(:cljs
     (:require-macros [gen.dynamic.trace])))

(defn no-op
  ([gf args]
   (apply gf args))
  ([_k gf args]
   (apply gf args)))

(def ^:dynamic *trace*
  "Applies the generative function gf to args. Dynamically rebound by functions
  like `gf/simulate`, `gf/generate`, `trace/update`, etc."
  no-op)

(def ^:dynamic *splice*
  "Applies the generative function gf to args. Dynamically rebound by functions
  like `gf/simulate`, `gf/generate`, `trace/update`, etc."
  no-op)

(defmacro without-tracing
  [& body]
  `(binding [*trace* no-op
             *splice* no-op]
     ~@body))

(deftype Trace [gf args subtraces retval score]
  trace/Args
  (args [_]
    args)

  trace/Choices
  (choices [_]
    (dynamic.choice-map/->ChoiceMap (update-vals subtraces trace/choices)))

  trace/GenFn
  (gf [_]
    gf)

  trace/RetVal
  (retval [_]
    @retval)

  #?@(:cljs [] ;; TODO cljs
      :clj
      [trace/Score
       (score [_]
              ;; TODO Handle untraced randomness.
              (if (realized? score)
                @score
                (do (deliver score (transduce (map trace/score)
                                              +
                                              0
                                              (vals subtraces)))
                    @score)))

       clojure.lang.Associative
       (containsKey [_ k]
                    (contains? subtraces k))
       (entryAt [_ k]
                (.entryAt ^clojure.lang.Associative subtraces k))

       clojure.lang.Counted
       (count [_]
              (count subtraces))

       clojure.lang.IFn
       (invoke [this k]
               (.valAt this k))

       clojure.lang.ILookup
       (valAt [this k]
              (get (trace/choices this) k))
       (valAt [this k not-found]
              (get (trace/choices this) k not-found))

       clojure.lang.IMapIterable
       (keyIterator [this]
                    (.iterator
                     ^java.lang.Iterable
                     (keys (trace/choices this))))
       (valIterator [this]
                    (.iterator
                     ^java.lang.Iterable
                     (vals (trace/choices this))))

       clojure.lang.Seqable
       (seq [this]
            (seq (trace/choices this)))

       java.lang.Iterable
       (iterator [this]
                 (.iterator
                  (let [^clojure.lang.Seqable choice-map (trace/choices this)]
                    ^java.lang.Iterable (.seq choice-map))))]))

(def promise
  #?(:cljs #(atom nil)
     :clj  clojure.core/promise))

(defn trace
  [gf args]
  (Trace. gf args {} (promise) (promise)))

(defn set-retval!
  [^Trace t retval]
  ;; TODO cljs
  #?(:clj  (deliver (.-retval t) retval)
     :cljs (swap! (.-retval t) retval))
  t)

(defn assoc-subtrace
  [^Trace t addr subt]
  (let [subtraces (.-subtraces t)]
    (when (contains? subtraces addr)
      (throw
       (ex-info "Value or subtrace already present at address. The same
                      address cannot be reused for multiple random choices."
                {:addr addr})))
    (Trace. (.-gf t)
            (.-args t)
            (assoc subtraces addr subt)
            (.-retval t)
            (.-score t))))

(defn merge-subtraces
  [^Trace t1 ^Trace t2]
  (reduce-kv assoc-subtrace
             t1
             (.-subtraces t2)))
