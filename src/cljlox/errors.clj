(ns cljlox.errors)

(defn report
  "Report an error"
  [line where message]
  (println (str "[line " line "] Error" where ": " message))
  (flush))