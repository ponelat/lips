 ;; lips.el --- A simple lisp
;;; Commentary: A simple lisp
;;;
;; Lips is a simple lisp that can evaluate +,-,*,/ only, but can do so with nested expressions (or s-expressions)

(setq lips/private/functions
  '(("add" . +)
     ("sub" . -)
     ("div" . /)
     ("mul" . *)))

(defun lips/private/get-fn (fn-str)
  (cdr (assoc fn-str lips/private/functions)))

;; string => tokens => sexp

;; PROGRAM = LSWP 1*SEXP LSWP
;; SEXP = OPEN EXP *(LSWP EXP) CLOSE
;; EXP = SEXP / NUMBER / SYMBOL
;; SYMBOL = 1*ALPHA
;; OPEN = (
;; CLOSE = )

(setq lips/private/tokens
  '((:whitespace .
      (lambda (char token rest)
        (eq char ? )))
     (:symbol .
       (lambda (char token rest) (and (>= char ?a) (<= char ?z))))
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
  (cdr (lips/gobble-tokens lips/private/tokens (string-to-list str) nil)))

(defun lips/gobble-sexp (tokens sexp-body)
  (let ((token (car tokens))
         (tokens-rest (cdr tokens)))
    (pcase token
      ;; Nested
      (`(:open . ,val)
        (let ((res (lips/gobble-sexp tokens-rest nil)))
          (let ((tokens-rest (car res))
                 (sexp (cdr res)))
            (lips/gobble-sexp tokens-rest (cons sexp sexp-body)))))
      (`(:close . ,val)     (cons tokens-rest (cons :sexp (reverse sexp-body))))
      (`(:symbol . ,val)     (lips/gobble-sexp tokens-rest (cons token sexp-body)))
      (`(:number . ,val)     (lips/gobble-sexp tokens-rest (cons token sexp-body)))
      (`(:whitespace . ,val) (lips/gobble-sexp tokens-rest sexp-body)))))


(defun lips/gobble-sexps (tokens sexps)
  (let ((token (car tokens))
         (tokens-rest (cdr tokens)))
    (pcase token
      (`(:open . ,val)
        (let ((res (lips/gobble-sexp tokens-rest nil)))
          (let ((tokens-rest (car res))
                 (sexp (cdr res)))
            (lips/gobble-sexps tokens-rest (cons sexp sexps)))))
      (`(:whitespace . ,val) (lips/gobble-sexps tokens-rest sexps))
      (_  (cons tokens (reverse sexps))))))

(defun lips/parse (str)
  (cdr (lips/gobble-sexps
         (lips/tokenize str)
         nil)))

(defun lips/eval-exp (ast)
  (pcase ast
    (`(:symbol . ,val) (lips/private/get-fn val))
    (`(:number . ,val) (string-to-number val))
    (`(:sexp . (,fn . ,exps))
      (format "->%s" exps)
      (apply
        (lips/eval-exp fn)
        (mapcar #'lips/eval-exp exps)))))

(defun lips-eval (str)
  (let ((ast (lips/parse str)))
    (let ((results (mapcar 'lips/eval-exp ast)))
      (car (last results)))))

(provide 'lips)
;;; lips.el ends here
