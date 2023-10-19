(ns cljlox.parser
  (:require [cljlox.errors :as errors]
            [cljlox.parser :as parser]))

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
  "Check if the next token is of the given type"
  [parser token-type]
  (let [tokens (:tokens parser)]
    (if (empty? tokens)
      false
      (if (isAtEnd parser)
        false
        (if (= token-type (:token-type (current parser)))
          true
          false)))))

(defn match
  [parser token-type]
  (if (check parser token-type)
    (advance parser)
    nil))

(defn consume
  [parser token-type]
  (if (check parser token-type)
    [(current parser) (advance parser)]
    nil))

(declare expression)
(declare declaration)
(declare statement)

(defn primary
  [parser]
  (let [token (current parser)]
    (case (:token-type token)
      :false [{:expr-type :literal :value false :type :boolean} (advance parser)]
      :true [{:expr-type :literal :value true :type :boolean} (advance parser)]
      :nil [{:expr-type :literal :value nil :type :nil} (advance parser)]
      :number [{:expr-type :literal :value (:literal (current parser)) :type :number} (advance parser)]
      :string [{:expr-type :literal :value (:literal (current parser)) :type :string} (advance parser)]
      :identifier [{:expr-type :variable :name (current parser)} (advance parser)]
      :left-paren (let [[inner forward] (expression (advance parser))]
                    (if-let [forward (match forward :right-paren)]
                      [{:expr-type :grouping :expression inner} forward]
                      (throw (error (current forward) "Expect ')' after expression."))))
      (throw (error (current parser) "Expect expression.")))))

(defn getArguments
  [parser]
  (loop [arguments nil
         forward parser]
    (if-let [forward (match forward :right-paren)]
      [forward (into [] (reverse arguments))]
      (let [[argument forward] (expression forward)]
        (if-let [forward (match forward :comma)]
          (recur (cons argument arguments) forward)
          (recur (cons argument arguments) forward))))))

(defn finishCall
  [callee parser]
  (-> parser
      (getArguments)
      ((fn [result] [{:expr-type :call :callee callee :arguments (second result)} (first result)]))))

(defn call
  [parser]
  (loop [[expr forward] (primary parser)]
    (if-let [forward (match forward :left-paren)]
      (recur (finishCall expr forward))
      [expr forward])))

(defn unary
  [parser]
  (let [forward (advance parser)
        token (previous forward)
        operator (:token-type token)]
    (case operator
      (:bang :minus) (let [[right forward] (unary forward)]
                       [{:expr-type :unary :token token :right right} forward])
      (call parser))))


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

(defn andExpression
  [parser]
  (loop [[expr forward] (equality parser)]
    (let [token (current forward)]
      (if-let [forward (match forward :and)]
        (let [[right forward] (equality forward)]
          (recur [{:expr-type :logical
                   :token token
                   :left expr
                   :right right}
                  forward]))
        [expr forward]))))


(defn orExpression
  [parser]
  (loop [[expr forward] (andExpression parser)]
    (let [token (current forward)]
      (if-let [forward (match forward :or)]
        (let [[right forward] (andExpression forward)]
          (recur [{:expr-type :logical
                   :token token
                   :left expr
                   :right right}
                  forward]))
        [expr forward]))))

(defn assignment
  [parser]
  (let [[expr forward] (orExpression parser)]
    (if-let [forward (match forward :equal)]
      (let [equals (previous forward)
            [value forward] (assignment forward)]
        (if (= (:expr-type expr) :variable)
          (let [name (:name expr)]
            [{:expr-type :assign :name name :value value} forward])
          (throw (error equals "Invalid assignment target."))))
      [expr forward])))

(defn expression
  [parser]
  (assignment parser))

(defn printStatement
  [parser]
  (let [[value forward] (expression (advance parser))]
    (if-let [forward (match forward :semicolon)]
      [{:stmt-type :print :expression value} forward]
      (throw (error (current forward) "Expect ';' after value.")))))

(defn expressionStatement
  [parser]
  (let [[value forward] (expression parser)]
    (if-let [forward (match forward :semicolon)]
      [{:stmt-type :expression :expression value} forward]
      (throw (error (current forward) "Expect ';' after expression.")))))

(defn varDeclaration
  [parser]
  (if-let [forward (match parser :identifier)]
    (let [name (previous forward)
          [initializer forward] (if (check forward :equal)
                                  (expression (advance forward))
                                  [nil forward])]
      (if-let [forward (match forward :semicolon)]
        [{:stmt-type :var :name name :initializer initializer} forward]
        (throw (error (current forward) "Expect ';' after variable declaration."))))
    (throw (error (current parser) "Expect variable name."))))

(defn block
  [parser]
  (loop [statements nil
         forward parser]
    (if (and (not (isAtEnd forward))
             (not (check forward :right-brace)))
      (let [[statement forward] (declaration forward)]
        (recur (cons statement statements) forward))
      (if-let [forward (match forward :right-brace)]
        [{:stmt-type :block :statements (into [] (reverse statements))} forward]
        (throw (error (current forward) "Expect '}' after block."))))))

(defn ifStatement
  [parser]
  (if-let [forward (match parser :left-paren)]
    (let [[condition forward] (expression forward)]
      (if-let [forward (match forward :right-paren)]
        (let [[thenBranch forward] (statement forward)]
          (if-let [forward (match forward :else)]
            (let [[elseBranch forward] (statement forward)]
              [{:stmt-type :if :condition condition :then-branch thenBranch :else-branch elseBranch} forward])
            [{:stmt-type :if :condition condition :then-branch thenBranch :else-branch nil} forward]))
        (throw (error (current forward) "Expect ')' after if condition."))))
    (throw (error (current parser) "Expect '(' after 'if'."))))

(defn whileStatement
  [parser]
  (if-let [forward (match parser :left-paren)]
    (let [[condition forward] (expression forward)]
      (if-let [forward (match forward :right-paren)]
        (let [[body forward] (statement forward)]
          [{:stmt-type :while :condition condition :body body} forward])
        (throw (error (current forward) "Expect ')' after while condition."))))
    (throw (error (current parser) "Expect '(' after 'while'."))))

(defn forStatement
  [parser]
  (if-let [forward (match parser :left-paren)]
    (let [[initializer forward]
          (case (:token-type (current forward))
            :semicolon [nil (advance forward)]
            :var (varDeclaration (advance forward))
            (expressionStatement forward))
          [condition forward] (if (not (check forward :semicolon))
                                (expression forward)
                                [nil forward])
          [increment forward] (if (not (check forward :right-paren))
                                (expression (advance forward))
                                [nil forward])]
      (if-let [forward (match forward :right-paren)]
        (let [[body forward] (statement forward)
              statements (if-let [init initializer]
                           (list init)
                           nil)
              body (if-let [inc increment]
                     {:stmt-type :block :statements [body inc]}
                     body)
              body (if-let [cond condition]
                     {:stmt-type :while :condition cond :body body}
                     {:stmt-type :while :condition {:expr-type :literal :value true :type :boolean} :body body})
              statements (cons body statements)]
          [{:stmt-type :block :statements (into [] (reverse statements))} forward])
        (throw (error (current forward) "Expect ')' after for clauses."))))
    (throw (error (current parser) "Expect '(' after 'for'."))))

(defn breakStatement
  [parser]
  (let [token (previous parser)]
    (if-let [forward (match parser :semicolon)]
      [{:stmt-type :break :token token} forward]
      (throw (error (current parser) "Expect ';' after 'break'.")))))

(defn returnStatement
  [parser]
  (let [keyword (previous parser)
        [value forward] (if (check parser :semicolon)
                          [nil parser]
                          (expression parser))]
    (if-let [forward (match forward :semicolon)]
      [{:stmt-type :return :name keyword :value value} forward]
      (throw (error (current parser) "Expect ';' after return value.")))))

(defn statement
  [parser]
  (let [token (current parser)]
    (case (:token-type token)
      :print (printStatement parser)
      :left-brace (block (advance parser))
      :if (ifStatement (advance parser))
      :while (whileStatement (advance parser))
      :for (forStatement (advance parser))
      :break (breakStatement (advance parser))
      :return (returnStatement (advance parser))
      (expressionStatement parser))))

(defn get-function-name
  [parser]
  (if-let [[name forward] (consume parser :identifier)]
    (if-let [forward (match forward :left-paren)]
      [forward {:name name :stmt-type :function}]
      (throw (error (current parser) "Expect '(' after function name.")))
    (throw (error (current parser) "Expect function name."))))

(defn get-function-parameters
  [[parser f]]
  (if-let [forward (match parser :right-paren)]
    [forward f]
    (loop [parameters nil
           forward parser]
      (if-let [[identifier forward] (consume forward :identifier)]
        (if-let [forward (match forward :comma)]
          (recur (cons identifier parameters) forward)
          (if-let [forward (match forward :right-paren)]
            (if (> (count parameters) 255)
              (throw (error (current forward) "The number of parameters must be less than 250."))
              [forward (assoc f :params (into [] (reverse (cons identifier parameters))))])
            (throw (error (current forward) "Expect ')' after parameters."))))
        (throw (error (current parser) "Expect parameter name."))))))

(defn get-function-body
  [[parser f]]
  (if-let [forward (match parser :left-brace)]
    (let [[body forward] (block forward)]
      [forward (assoc f :body body)])
    (throw (error (current parser) "Expect '{' before function body."))))

(defn function
  [parser _]
  (-> parser
      (get-function-name)
      (get-function-parameters)
      (get-function-body)
      ((fn [[forward f]] [f forward]))))

(defn declaration
  [parser]
  (if-let [forward (match parser :var)]
    (varDeclaration forward)
    (if-let [forward (match parser :fun)]
      (function forward :function)
      (statement parser))))

(defn parse
  "Parse tokens"
  [tokens]
  (loop [parser {:tokens tokens :position 0}
         statements []]
    (if (isAtEnd parser)
      (into [] (reverse statements))
      (let [[next forward] (declaration parser)]
        (recur forward (cons next statements))))))
