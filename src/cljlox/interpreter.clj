(ns cljlox.interpreter
  (:require [cljlox.errors :as errors]))

(defn checkNumericOperand
  [operand]
  (if (= (type operand) java.lang.Double)
    operand
    nil))

(defmulti interpret (fn [expr] (:expr-type expr)))

(defmethod interpret :literal [expr]
  (:value expr))

(defmethod interpret :grouping [expr]
  (interpret (:expression expr)))

(defmethod interpret :unary [expr]
  (defn isTruthy? [value]
    (case value
      nil false
      false false
      true))
  (let [right (interpret (:right expr))
        token (:token expr)
        operator (:token-type token)]
    (case operator
      :minus (if-let [operand (checkNumericOperand right)]
               (- operand)
               (throw (ex-info "Operand must be a number." {:token token})))
      :bang (not (isTruthy? right)))))

(defmethod interpret :binary [expr]
  (let [left (interpret (:left expr))
        right (interpret (:right expr))
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

(defmethod interpret :comparison [expr]
  (let [left (interpret (:left expr))
        right (interpret (:right expr))
        token (:token expr)
        operator (:token-type token)]
    (case operator
      :greater (> left right)
      :greater-equal (>= left right)
      :less (< left right)
      :less-equal (<= left right))))

(defmethod interpret :equality [expr]
  (defn isEqual
    [a b]
    (case [a b]
      [nil nil] true
      [nil _] false
      (= a b)))
  (let [left (interpret (:left expr))
        right (interpret (:right expr))
        token (:token expr)
        operator (:token-type token)]
    (case operator
      :bang-equal (not (isEqual left right))
      :equal-equal (isEqual left right))))