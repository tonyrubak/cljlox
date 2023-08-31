(ns cljlox.scanner
  (:require [cljlox.errors :as errors]))

(defn error
  "Report an error"
  [line message]
  (errors/report line "" message)
  (ex-info message {:line line}))

(def keywords
  ["and" "class" "else" "false" "for" "fun" "if" "nil" "or" "print" "return"
   "super" "this" "true" "var" "while"])

(defn number
  [{line :line, char-list :char-list}]
  (let [whole-part (take-while #(Character/isDigit %) char-list)
        fractional-part (if (= (first (drop (count whole-part) char-list)) \.)
                          (cons \. (take-while #(Character/isDigit %) (drop (inc (count whole-part)) char-list)))
                          [])
        number (concat whole-part fractional-part)]
    [{:token-type :number
      :lexeme (apply str number)
      :literal (Double/parseDouble (apply str number))
      :line line}
     {:line line :char-list (drop (count number) char-list)}]))

(defn isStartOfIdentifier
  [char]
  (or (Character/isLetter char)
      (= char \_)))

(defn isIdentifierSymbol
  [char]
  (or (Character/isLetterOrDigit char)
      (= char \_)))

(defn identifier
  [{line :line, char-list :char-list}]
  (let [identifier (take-while isIdentifierSymbol char-list)]
    [{:token-type (if (some #(= % (apply str identifier)) keywords)
                    (keyword (apply str identifier))
                    :identifier)
      :lexeme (apply str identifier)
      :line line}
     {:line line :char-list (drop (count identifier) char-list)}]))

(defn string
  [scanner]
  (let [start-line (:line scanner)]
    (loop [acc ""
           {line :line, char-list :char-list} scanner]
      (case (first char-list)
        nil (error line "Unterminated string.")
        \" [{:token-type :string :lexeme (str "\"" acc "\"") :literal acc :line start-line} {:line line :char-list (rest char-list)}]
        \newline (recur (str acc (first char-list) \n) {:line (inc line) :char-list (rest char-list)})
        (recur (str acc (first char-list)) {:line line :char-list (rest char-list)})))))

(defn scanToken
  "Scan tokens"
  [{line :line, char-list :char-list}]
  (case (first char-list)
    \( [{:token-type :left-paren :lexeme (str (first char-list)) :line line} {:line line :char-list (rest char-list)}]
    \) [{:token-type :right-paren :lexeme (str (first char-list)) :line line} {:line line :char-list (rest char-list)}]
    \{ [{:token-type :left-brace :lexeme (str (first char-list)) :line line} {:line line :char-list (rest char-list)}]
    \} [{:token-type :right-brace :lexeme (str (first char-list)) :line line} {:line line :char-list (rest char-list)}]
    \, [{:token-type :comma :lexeme (str (first char-list)) :line line} {:line line :char-list (rest char-list)}]
    \. [{:token-type :dot :lexeme (str (first char-list)) :line line} {:line line :char-list (rest char-list)}]
    \- [{:token-type :minus :lexeme (str (first char-list)) :line line} {:line line :char-list (rest char-list)}]
    \+ [{:token-type :plus :lexeme (str (first char-list)) :line line} {:line line :char-list (rest char-list)}]
    \; [{:token-type :semicolon :lexeme (str (first char-list)) :line line} {:line line :char-list (rest char-list)}]
    \* [{:token-type :star :lexeme (str (first char-list)) :line line} {:line line :char-list (rest char-list)}]
    \! (if (= (first (rest char-list)) \=)
         [{:token-type :bang-equal :lexme (apply str (take 2 char-list)) :line line} {:line line :char-list (drop 2 char-list)}]
         [{:token-type :bang :lexeme (str (first char-list)) :line line} {:line line :char-list (rest char-list)}])
    \= (if (= (first (rest char-list)) \=)
         [{:token-type :equal-equal :lexme (apply str (take 2 char-list)) :line line} {:line line :char-list (drop 2 char-list)}]
         [{:token-type :equal :lexeme (str (first char-list)) :line line} {:line line :char-list (rest char-list)}])
    \< (if (= (first (rest char-list)) \=)
         [{:token-type :less-equal :lexme (apply str (take 2 char-list)) :line line} {:line line :char-list (drop 2 char-list)}]
         [{:token-type :less :lexeme (str (first char-list)) :line line} {:line line :char-list (rest char-list)}])
    \> (if (= (first (rest char-list)) \=)
         [{:token-type :greater-equal :lexme (apply str (take 2 char-list)) :line line} {:line line :char-list (drop 2 char-list)}]
         [{:token-type :greater :lexeme (str (first char-list)) :line line} {:line line :char-list (rest char-list)}])
    \/ (if (= (first (rest char-list)) \/)
         [{:token-type :comment :lexeme (apply str (take-while #(not= % \newline) char-list)) :line line} {:line line :char-list (drop-while #(not= % \newline) char-list)}]
         [{:token-type :slash :lexeme (str (first char-list)) :line line} {:line line :char-list (rest char-list)}])
    \" (string {:line line :char-list (rest char-list)})
    (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) (number {:line line :char-list char-list})
    (\space \tab) [{:token-type :ws :line line} {:line line :char-list (rest char-list)}]
    \newline [{:token-type :ws} {:line (inc line) :char-list (rest char-list)}]
    (if (isStartOfIdentifier (first char-list))
      (identifier {:line line :char-list char-list})
      (error line (str "Unexpected character " (first char-list))))))

(defn scanTokens
  [source]
  (loop [tokens []
         scanner {:line 0 :char-list (seq source)}]
    (let [[token forward] (scanToken scanner)]
      (if (empty? (:char-list forward))
        (let [tokens (cons token tokens)
              line (:line (first tokens))
              tokens (cons {:token-type :eof :line line} tokens)]
          (-> tokens
              reverse
              (#(filter (fn [item] (not= (:token-type item) :ws)) %))
              (#(into [] %))
              ))
        (recur (cons token tokens) forward)))))