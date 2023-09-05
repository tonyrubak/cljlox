(ns cljlox.environment)

(defn define
  "Define a variable in the environment"
  [env name value]
  (swap! env #(assoc % name value)))

(defn lookup
  "Get a variable from the environment"
  [env name]
  (if-let [value (get @env name)]
    value
    (throw (ex-info (str "Undefined variable '" name "'.") {:name name}))))