;;; -*- lexical-binding: t -*-
;; lips.el --- A simple lisp
;;; Commentary: A simple lisp
;;;
;; Lips is a simple lisp that can evaluate +,-,*,/ only, but can do so with nested expressions (or s-expressions)



(defun lips/make-hash (&optional key-values)
  "Makes a `hash-table' with the test = `equal' suitable for string keys.
The BODY is a list of key value sequences."
  (let ((hash (make-hash-table :test 'equal)))
    (-each (-partition 2 key-values)
      (lambda (pair) (puthash (nth 0 pair) (nth 1 pair) hash)))
    hash))

(setq lips/core-symbols
  (lips/make-hash
    '("+" +
       "-" -
       "nil" nil
       "=" equal
       "true" t
       "/" /
       "*" *)))

(defun lips/maps-find-first (maps key)
  "Takes a list of MAPS and find the first KEY with a value, return the value."
  (if maps
    (let ((head (car maps))
           (rest (cdr maps)))
      (let ((val (gethash key head)))
        (if val val
          (lips/maps-find-first rest key))))))

;; string => tokens => form

;; PROGRAM = LSWP 1*SEXP LSWP
;; SEXP = OPEN EXP *(LSWP EXP) CLOSE
;; EXP = SEXP / NUMBER / SYMBOL
;; SYMBOL = 1*ALPHA
;; OPEN = (
;; CLOSE = )

(setq lips/tokens
  '((:whitespace .
      (lambda (char token rest)
        (or
          (eq char ? )
          (eq char 10)
          (eq char 13))))
     (:symbol .
       (lambda (char token rest)
         (or
           (= char ?+)
           (= char ?-)
           (= char ?*)
           (= char ?/)
           (= char ?=)
           (and (>= char ?a) (<= char ?z)))))
     (:number .
       (lambda (char token rest) (and (>= char ?0) (<= char ?9))))
     (:open .
       (lambda (char token rest)
         (unless token
           (eq char 40 ))))
     (:close .
       (lambda (char token rest)
         (unless token
           (eq char 41 ))))))

(defun lips/gobble-char (pred chars token)
  "Using PRED function to gobble from CHARS list a TOKEN or nil.
Returns a cons cell of (remaining-chars . token) or (CHARS . nil)."
  (let ((char (car chars))
         (rest (cdr chars)))
    (if (funcall pred char token rest)
      (if rest
        (lips/gobble-char pred rest (cons char token))
        (cons rest (reverse (cons char token))))
      (cons chars (reverse token)))))

(defun lips/gobble-token (fns chars)
  "fns = '((fn . :token) (fn . :token))"
  (let ((token-name (car (car fns)))
         (fn (cdr (car fns)))
         (fn-rest (cdr fns)))
    (if fn
      (let ((res (lips/gobble-char fn chars nil)))
        (let ((chars-rest (car res))
               (token (cdr res)))
          (if token
            (cons chars-rest (cons token-name (concat token)))
            (lips/gobble-token (cdr fns) chars))))
      (cons chars nil))))

(defun lips/gobble-tokens (fns chars tokens)
  (if chars
    (let ((res (lips/gobble-token fns chars)))
      (let ((chars-rest (car res))
             (token (cdr res)))
        (if token
          (lips/gobble-tokens fns chars-rest (cons token tokens))
          (cons chars (reverse tokens)))))
    (cons nil (reverse tokens))))



(defun lips/tokenize (str)
  "Returns a list of tokens (symbols) of "
  (cdr (lips/gobble-tokens lips/tokens (string-to-list str) nil)))

(defun lips/gobble-forms (tokens body)
  (if tokens
    (let ((token (car tokens))
           (tokens-rest (cdr tokens)))
      (pcase token
        ;; Nested
        (`(:open . ,val)
          (let ((res (lips/gobble-forms tokens-rest nil)))
            (let ((tokens-rest (car res))
                   (form (cdr res)))
              (lips/gobble-forms tokens-rest (cons form body)))))
        (`(:close . ,val)     (cons tokens-rest (cons :list (reverse body))))
        (`(:symbol . ,val)     (lips/gobble-forms tokens-rest (cons token body)))
        (`(:number . ,val)     (lips/gobble-forms tokens-rest (cons token body)))
        (`(:whitespace . ,val) (lips/gobble-forms tokens-rest body))))
    body))

(defun lips/parse (str)
  (reverse (lips/gobble-forms (lips/tokenize str) nil)))

(defun lips/eval-exp (scopes ast)
  (pcase ast
    (`(:symbol . ,key) (lips/maps-find-first scopes key))
    (`(:number . ,val) (string-to-number val))
    (`(:list . (,f . ,exps))
      (pcase f
        (`(:symbol . "def")
          (let ((key (cdr (nth 0 exps)))
                 (val (lips/eval-exp scopes (nth 1 exps))))
            (puthash key val (car scopes))
            val))
        (`(:symbol . "cond")
          (lips/eval-exp
            scopes
            (car
              (cdr
                (-find
                  (lambda (pair)
                    (lips/eval-exp scopes (car pair)))
                  (-partition 2 exps))))))
        (`(:symbol . "if")
          (let ((pred (nth 0 exps))
                 (true-clause (nth 1 exps))
                 (false-clause (nth 2 exps)))
            (if
              (lips/eval-exp scopes pred)
              (lips/eval-exp scopes true-clause)
              (lips/eval-exp scopes false-clause))))
        (`(:symbol . "fn")
          (let ((params (-clone (cdr (car exps))))
                 (body (-clone (cdr exps))))
            (lambda (&rest vargs)
              (let ((scope (lips/make-hash)))
                (-each-indexed params
                  (lambda (i arg) (puthash (cdr arg) (nth i vargs) scope)))
                (lips/eval-exp (cons scope scopes) (car body))))))
        (`(:symbol . "list")
          (mapcar (lambda (exp) (lips/eval-exp scopes exp)) exps))
        (_ (apply
            (lips/eval-exp scopes f)
             (mapcar (lambda (exp) (lips/eval-exp scopes exp)) exps)))))
        ;; Else return the thing itself
    (_ ast)))


(defun lips-eval (str &optional user-scope)
  (let ((ast (lips/parse str))
         (scopes (list
                   (lips/make-hash user-scope)
                   lips/core-symbols)))
    (let ((results (mapcar (lambda (exp) (lips/eval-exp scopes exp)) ast)))
      (car (last results)))))

(defun lips/test (str expected &optional scope)
  (let ((actual (lips-eval str scope)))
    (if (equal actual expected) nil
      (format "Failed on \"%s\": actual => %s (exptected %s) %s"
        str actual expected
        (if scope (format " - with scope %s" scope) "")))))
(or
  ;; cond
  (lips/test "(cond (= 1 0) 1 (= 1 1) 2)" 2)
  ;; equals false
  (lips/test "(= 1 0)" nil)
  ;; equals (and true)
  (lips/test "(= 1 1)" t)
  ;; If - true
  (lips/test "(if 1 (def a 1) (def b 1)) a" 1)
  ;; If - true
  (lips/test "(if 1 (def a 1) (def b 1)) a" 1)
  ;; If - desn't eval false
  (lips/test "(if nil (def a 1)) a" nil)
  ;; If - false
  (lips/test "(if nil 1 2)" 2)
  ;; Function
  (lips/test "((fn (a b) (+ a b)) 1 2)" 3)
  ;; Def
  (lips/test "(def a 4) (+ a 2)" 6)
  ;; List
  (lips/test "(list 1 2 3)" '(1 2 3))
  ;; Nested
  (lips/test "(+ 10 (* 10 10))" 110)
  ;; User provided scope
  (lips/test "(+ 10 (* 10 10))" 30 '("*" +)))

(provide 'lips)
;;; lips.el ends here
