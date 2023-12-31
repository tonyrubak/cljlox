(ns cljlox.core
  (:gen-class)
  (:require [cljlox.parser :as parser]
            [cljlox.scanner :as scanner]
            [cljlox.interpreter :as interpreter]
            [cljlox.environment :as environment]))

(defn run
  "Run a string"
  ([source] (run source (interpreter/initialize)))
  ([source env]
   (try
     (-> source
         (scanner/scanTokens)
         (parser/parse)
         (#(interpreter/run % env)))
     (catch Exception _))))

(defn runFile
  "Run a file"
  [path]
  (let [bytes (slurp path)]
    (run bytes)))

(defn runPrompt
  "Run a prompt"
  []
  (let [env (interpreter/initialize)]
    (loop []
      (print "> ")
      (flush)
      (when-let [line (read-line)]
        (run line env)
        (recur)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (cond
    (> (count args) 1) (println "Usage: jlox [script]")
    (= (count args) 1) (runFile (first args))
    :else (runPrompt)))