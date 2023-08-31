(ns cljlox.parser
  (:require [cljlox.errors :as errors]))

(defmulti error (fn [token _] (:token-type token)))

(defmethod error :eof [token message]
  (errors/report (:line token) " at end" message)
  (ex-info message {:token token}))

(defmethod error :default [token message]
  (errors/report (:line token) (str " at '" (:lexeme token) "'") message)
  (ex-info message {:token token}))

(defn current
  "Get the current token"
  [{tokens :tokens, position :position}]
  (if (empty? tokens)
    nil
    (nth tokens position)))

(defn previous
  "Get the previous token"
  [{tokens :tokens, position :position}]
  (if (empty? tokens)
    nil
    (nth tokens (dec position))))

(defn isAtEnd
  "Check if we are at the end of the tokens"
  [{tokens :tokens, position :position}]
  (= (:token-type (current {:tokens tokens :position position})) :eof))

(defn advance
  "Advance to the next character"
  [scanner]
  (let [{:keys [tokens position]} scanner]
    (if (empty? tokens)
      nil
      (if (isAtEnd scanner)
        scanner
        {:tokens tokens :position (inc position)}))))

(defn check
  "Check if the next character is a given character"
  [{tokens :tokens, position :position} type]
  (if (empty? tokens)
    false
    (if (isAtEnd {:tokens tokens :position position})
      false
      (= type (:token-type (current {:tokens tokens :position position}))))))

(declare expression)

(defn consume
  [parser token-type]
  (if (check parser token-type)
    [:ok (advance parser)]
    [:error parser]))

(defn primary
  [parser]
  (let [token (current parser)]
    (case (:token-type token)
      :false [{:expr-type :literal :value false :type :boolean} (advance parser)]
      :true [{:expr-type :literal :value true :type :boolean} (advance parser)]
      :nil [{:expr-type :literal :value nil :type :nil} (advance parser)]
      :number [{:expr-type :literal :value (:literal (current parser)) :type :number} (advance parser)]
      :string [{:expr-type :literal :value (:literal (current parser)) :type :string} (advance parser)]
      :left-paren (let [[inner forward] (expression (advance parser))]
                    (case (first (consume forward :right-paren))
                      :ok [{:expr-type :grouping :expression inner} (advance forward)]
                      :error (throw (error (current forward) "Expect ')' after expression."))))
      (throw (error (current parser) "Expect expression.")))))

(defn unary
  [parser]
  (let [forward (advance parser)
        token (previous forward)
        operator (:token-type token)]
    (case operator
      (:bang :minus) (let [[right forward] (unary forward)]
                       [{:expr-type :unary :token token :right right} forward])
      (primary parser))))

(defn factor
  [parser]
  (loop [[expr forward] (unary parser)]
    (let [token (current forward)
          operator (:token-type token)]
      (case operator
        (:star :slash) (let [[right forward] (unary (advance forward))]
                         (recur [{:expr-type :binary
                                  :token token
                                  :left expr
                                  :right right}
                                 forward]))
        [expr forward]))))

(defn term
  [parser]
  (loop [[expr forward] (factor parser)]
    (let [token (current forward)
          operator (:token-type token)]
      (case operator
        (:minus :plus) (let [[right forward] (factor (advance forward))]
                         (recur [{:expr-type :binary
                                  :token token
                                  :left expr
                                  :right right}
                                 forward]))
        [expr forward]))))

(defn comparison
  [parser]
  (loop [[expr forward] (term parser)]
    (let [token (current forward)
          operator (:token-type token)]
      (case operator
        (:greater :greater-equal :less :less-equal)
        (let [[right forward] (term (advance forward))]
          (recur [{:expr-type :comparison
                   :token token
                   :left expr
                   :right right}
                  forward]))
        [expr forward]))))

(defn equality
  [parser]
  (loop [[expr forward] (comparison parser)]
    (let [token (current forward)
          operator (:token-type token)]
      (case operator
        (:bang-equal :equal-equal)
        (let [[right forward] (comparison (advance forward))]
          (recur [{:expr-type :equality
                   :token token
                   :left expr
                   :right right}
                  forward]))
        [expr forward]))))

(defn expression
  [parser]
  (equality parser))

(defn parse
  "Parse tokens"
  [tokens]
  (try
    (first (expression {:tokens tokens :position 0}))
    (catch Exception _
      nil)))