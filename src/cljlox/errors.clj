(ns cljlox.errors)

(defn report
  "Report an error"
  [line where message]
  (println (str "[line " (inc line) "] Error" where ": " message))
  (flush))

(defn runtimeError
  "Report a runtime error"
  [token message]
  (report (:line token) "" message)
  (flush))
