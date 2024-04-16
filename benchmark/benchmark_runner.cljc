(ns benchmark-runner
  "Benchmark runner."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn find-namespaces-in-directory
  "Returns namespace names for Clojure files in supplied `dir`."
  [dir]
  (let [namespace-prefix "gen"
        base-dir (io/file dir)]
    (->> (file-seq base-dir)
         (filter #(.endsWith (.getName %) ".cljc"))
         (map #(.getPath %))
         (map #(str/replace % (str base-dir) ""))
         (map #(str/replace % #"[\\/]" "."))
         (map #(str/replace % "_" "-"))
         (map #(str/replace % #"\.cljc$" ""))
         (map #(str/trim %))
         (map #(str namespace-prefix %)))))

(defn load-namespaces
  "Dynamically loads a list of namespaces."
  [namespaces]
  (doseq [ns namespaces]
    (try
      (require (symbol ns) :reload)
      (catch Exception e
        (println "Error loading namespace:" ns ": " (.getMessage e))))))

(defn benchmark-functions
  "Returns a list of functions tagged as :benchmark in the given namespace."
  [namespace]
  (->> (ns-publics (find-ns (symbol namespace)))
       (keep (fn [[sym var]] (when (:benchmark (meta var)) sym)))))

(defn run-benchmarks
  "Finds and runs all benchmark functions."
  []
  (let [namespaces (find-namespaces-in-directory "benchmark/gen")]
    (load-namespaces namespaces)
    (doseq [ns namespaces
            fn-name (benchmark-functions ns)]
      (when-let [qualified-fn (resolve (symbol (str ns "/" fn-name)))]
        (println "Running benchmark for" ns "/" fn-name)
        (qualified-fn)))))

(defn -main []
  (run-benchmarks))
