(ns cljlox.core
  (:gen-class)
  (:require [cljlox.parser :as parser]
            [cljlox.scanner :as scanner]
            [cljlox.interpreter :as interpreter]))

(defn run
  "Run a string"
  [source]
  (try
    (-> source
        (scanner/scanTokens)
        (parser/parse)
        (interpreter/run)) 
    (catch Exception _)))

(defn runFile
  "Run a file"
  [path]
  (let [bytes (slurp path)]
    (run bytes)))

(defn runPrompt
  "Run a prompt"
  []
  (loop []
    (print "> ")
    (flush)
    (when-let [line (read-line)]
      (run line)
      (recur))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (cond
    (> (count args) 1) (println "Usage: jlox [script]")
    (= (count args) 1) (runFile (first args))
    :else (runPrompt)))