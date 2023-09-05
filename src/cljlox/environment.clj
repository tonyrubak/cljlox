(ns cljlox.environment)

(defn create
  "Create a new environment"
  ([] (create nil))
  ([enclosing] (atom {:system/enclosing enclosing})))

(defn define
  "Define a variable in the environment"
  [env name value]
  (swap! env #(assoc % name value)))

(defn lookup
  "Get a variable from the environment"
  [env name]
  (if-let [value (get @env name)]
    value
    (if-let [enclosing (:system/enclosing @env)]
      (lookup enclosing name)
      (throw (ex-info (str "Undefined variable '" name "'.") {:name name})))))

(defn assign
  "Assign a variable in the environment"
  [env name value]
  (if (contains? @env name)
    (do
      (swap! env #(assoc % name value))
      value)
    (if-let [enclosing (:system/enclosing @env)]
      (assign enclosing name value)
      (throw (ex-info (str "Undefined variable '" name "'.") {:name name})))))