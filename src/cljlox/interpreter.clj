(ns cljlox.interpreter
  (:require [cljlox.errors :as errors]
            [cljlox.environment :as environment]))

(defn checkNumericOperand
  [operand]
  (if (= (type operand) java.lang.Double)
    operand
    nil))

(defmulti interpret (fn [expr _] (if-let [expr-type (:expr-type expr)]
                                     expr-type
                                     (:statement-type expr))))

(defmethod interpret :var [expr env]
  (if-let [initializer (:initializer expr)]
    (let [value (interpret initializer env)]
      (environment/define env (:lexeme (:name expr)) value))
    (environment/define env (:lexeme (:name expr)) nil)))

(defmethod interpret :expression [expr env]
  (interpret (:expression expr) env))

(defmethod interpret :print [expr env]
  (println (interpret (:expression expr) env))
  (flush))

(defmethod interpret :variable [expr env]
  (environment/lookup env (:lexeme (:name expr))))

(defmethod interpret :literal [expr _]
  (:value expr))

(defmethod interpret :grouping [expr env]
  (interpret (:expression expr) env))

(defmethod interpret :unary [expr env]
  (defn isTruthy? [value]
    (case value
      nil false
      false false
      true))
  (let [right (interpret (:right expr) env)
        token (:token expr)
        operator (:token-type token)]
    (case operator
      :minus (if-let [operand (checkNumericOperand right)]
               (- operand)
               (throw (ex-info "Operand must be a number." {:token token})))
      :bang (not (isTruthy? right)))))

(defmethod interpret :binary [expr env]
  (let [left (interpret (:left expr) env)
        right (interpret (:right expr) env)
        token (:token expr)
        operator (:token-type token)]
    (case operator
      :minus (if-let [right (checkNumericOperand right)]
               (if-let [left (checkNumericOperand left)]
                 (- left right)
                 (throw (ex-info "Operands must be numbers." {:token token})))
               (throw (ex-info "Operands must be numbers." {:token token})))
      :slash (if-let [right (checkNumericOperand right)]
               (if-let [left (checkNumericOperand left)]
                 (/ left right)
                 (throw (ex-info "Operands must be numbers." {:token token})))
               (throw (ex-info "Operands must be numbers." {:token token})))
      :star (if-let [right (checkNumericOperand right)]
              (if-let [left (checkNumericOperand left)]
                (* left right)
                (throw (ex-info "Operands must be numbers." {:token token})))
              (throw (ex-info "Operands must be numbers." {:token token})))
      :plus (if (= (type left) java.lang.Double)
              (if (= (type right) java.lang.Double)
                (+ left right)
                (throw (ex-info "Operands must be numbers." {:token token})))
              (if (= (type left) java.lang.String)
                (if (= (type right) java.lang.String)
                  (str left right)
                  (throw (ex-info "Operands must be strings." {:token token})))
                (throw (ex-info "Operands must be two numbers or two strings." {:token token})))))))

(defmethod interpret :comparison [expr env]
  (let [token (:token expr)
        operator (:token-type token)]
    (if-let [right (checkNumericOperand (interpret (:right expr) env))]
      (if-let [left (checkNumericOperand (interpret (:left expr) env))]
        (case operator
          :greater (> left right)
          :greater-equal (>= left right)
          :less (< left right)
          :less-equal (<= left right))
        (throw (ex-info "Operands must be numbers." {:token token})))
      (throw (ex-info "Operands must be numbers." {:token token})))))

(defmethod interpret :equality [expr env]
  (defn isEqual
    [a b]
    (case [a b]
      [nil nil] true
      [nil _] false
      (= a b)))
  (let [left (interpret (:left expr) env)
        right (interpret (:right expr) env)
        token (:token expr)
        operator (:token-type token)]
    (case operator
      :bang-equal (not (isEqual left right))
      :equal-equal (isEqual left right))))

(defn run
  [statements env]
  (doseq [statement statements]
    (try
      (interpret statement env)
      (catch Exception e
        (errors/runtimeError (:token e) (:message e))))))