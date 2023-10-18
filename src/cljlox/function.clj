(ns cljlox.function
  (:require [cljlox.callable :as callable]
            [cljlox.environment :as environment]))


(defrecord LoxFunction [declaration closure]
  callable/LoxCallable
  (call [_ interpret-method arguments]
    (let [env (environment/create closure)]
      (doseq [[param arg] (map vector (:params declaration) arguments)]
        (environment/define env (:lexeme param) arg))
      (try
        (interpret-method (:body declaration) env)
        nil
        (catch Exception e
          (if (= :return (-> e ex-data :cause))
            (-> e ex-data :value)
            (throw e))))))
  (arity [_]
    (count (:params declaration)))
  (toString [_]
    (str "<fn " (:lexeme (:name declaration)) ">")))
