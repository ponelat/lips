 ;; lips.el --- A simple lisp
;;; Commentary: A simple lisp
;;;
;; Lips is a simple lisp that can evaluate +,-,*,/ only, but can do so with nested expressions (or s-expressions)

(setq lips/core-symbols
  `(
     ("def" .
       (lambda (&rest args)
         (message (format "-> %s" args))))
     ("fn" .
       (lambda (&rest args)
         "fn!"))
     ("+" . +)
     ("add" . +)
     ("-" . -)
     ("sub" . -)
     ("/" . /)
     ("div" . /)
     ("*" . *)
     ("mul" . *)))

(defun lips/find-first-assoc (alists key)
  "Takes a list of ALISTS and find the first KEY with a value."
  (if alists
    (let ((alist (car alists ))
           (alist-rest (cdr alists)))
      (let ((val (cdr (assoc key alist))))
        (if val val
          (lips/find-first-assoc alist-rest key))))))

;; string => tokens => sexp

;; PROGRAM = LSWP 1*SEXP LSWP
;; SEXP = OPEN EXP *(LSWP EXP) CLOSE
;; EXP = SEXP / NUMBER / SYMBOL
;; SYMBOL = 1*ALPHA
;; OPEN = (
;; CLOSE = )

(setq lips/tokens
  '((:whitespace .
      (lambda (char token rest)
        (eq char ? )))
     (:symbol .
       (lambda (char token rest)
         (or
           (= char ?+)
           (= char ?-)
           (= char ?*)
           (= char ?/)
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

(defun lips/gobble-forms (tokens sexp-body)
  (if tokens
    (let ((token (car tokens))
           (tokens-rest (cdr tokens)))
      (pcase token
        ;; Nested
        (`(:open . ,val)
          (let ((res (lips/gobble-forms tokens-rest nil)))
            (let ((tokens-rest (car res))
                   (sexp (cdr res)))
              (lips/gobble-forms tokens-rest (cons sexp sexp-body)))))
        (`(:close . ,val)     (cons tokens-rest (cons :sexp (reverse sexp-body))))
        (`(:symbol . ,val)     (lips/gobble-forms tokens-rest (cons token sexp-body)))
        (`(:number . ,val)     (lips/gobble-forms tokens-rest (cons token sexp-body)))
        (`(:whitespace . ,val) (lips/gobble-forms tokens-rest sexp-body))))
    sexp-body))

(defun lips/parse (str)
  (lips/gobble-forms (lips/tokenize str) nil))

(defun lips/eval-exp (scopes ast)
  (pcase ast
    (`(:symbol . ,key) (lips/find-first-assoc scopes key))
    (`(:number . ,val) (string-to-number val))
    (`(:sexp . (,fn . ,exps))
      (pcase fn
        (`(:symbol . "def")
          "def!")
        (`(:symbol . "fn")
          "fn!")
        (_ (apply
           (lips/eval-exp scopes fn)
             (mapcar (lambda (exp) (lips/eval-exp scopes exp)) exps)))))))

(defun lips-eval (str)
  (let ((ast (lips/parse str))
         (scopes (list lips/core-symbols)))
    (let ((results (mapcar (lambda (exp) (lips/eval-exp scopes exp)) ast)))
      (car (last results)))))

;; (lips-eval "((fn (a b) (+ a b)) 6 7)")
;; (lips-eval "1")
;; (lips-eval "(def a 1) (+ 1 a)")
;; (lips-eval "(+ 1 (- 2 1))")

(provide 'lips)
;;; lips.el ends here
