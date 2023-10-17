(ns cljlox.function
  (:require [cljlox.callable :as callable]
            [cljlox.environment :as environment]))


(defrecord LoxFunction [declaration]
  callable/LoxCallable
  (call [_ interpreter arguments]
    (let [env (environment/create (:globals interpreter))]
      (doseq [[param arg] (map vector (:params declaration) arguments)]
        (environment/define env (:lexeme param) arg))
      ((:fn interpreter) (:body declaration) env)
      nil))
  (arity [_]
    (count (:params declaration)))
  (toString [_]
    (str "<fn " (:lexeme (:name declaration)) ">")))
