(ns cljlox.callable)

(defprotocol LoxCallable
  (call [this interpreter arguments])
  (arity [this]))
