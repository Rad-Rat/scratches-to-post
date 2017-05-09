
(error "do not load this file!")

;;;; WARN:

;;; ==================================================================
;;;                   THIS IS OBSOLETE FILE  
;;;
;;;                see el-slime-live-highlight.el
;;;              (That is slightly better than this)
;;; ==================================================================


;;;; Common lisp part of code
;; ================================================================
(defpackage cl-slime-live-highlight
  (:use :cl)
  (:documentation
   "This is part of rat/slime-high "))

(in-package :cl-slime-live-highlight)

;; variable special macro function
(defun cl/slime-extract-symbols (seq)
   (loop with res = nil
      for i in seq
      do (setq res (let ((symb (find-symbol (string-upcase (third i)))))
                     (when symb
                       (cond ((boundp symb) "variable")
                             ((special-operator-p symb) "special")
                             ((macro-function symb) "macro")
                             ((fboundp symb) "function")
                             (t nil)))))
      when res
      collect `(,(first i)
                 ,(second i)
                 ,res)))

;;;; Emacs lisp part of code
;; ================================================================
;;; Faces
(defface shigh-functions
    '((t (:foreground "#00006DE06DE0"))) ; Like `font-lock-constant-face'.  Do not inherit.
  "Face used to highlight Lisp functions."
  :group 'Slime-Highlight :group 'faces)

(defvar shigh-variables-face 'font-lock-variable-name-face)
(defvar shigh-functions-face 'font-lock-function-name-face)
(defvar shigh-macros-face 'shigh-functions)
(defvar shigh-special-face 'font-lock-keyword-face)

;;; define-minore-mode
(define-minor-mode slime-live-highlight-mode
    "no documentation"
  :group 'Slime-Highlight
  (if slime-live-highlight-mode
      (progn (font-lock-add-keywords nil
                                     '((el/slime-live-fontify-function 0 (get-el/shigh-face))) 'append)
             (jit-lock-register #'el/shigh-live--jit-lock-prepare t)
             (font-lock-flush))
    (font-lock-remove-keywords nil
                               '((el/slime-live-fontify-function 0 (get-el/shigh-face))))
    (jit-lock-unregister #'el/shigh-live--jit-lock-prepare)))

;;; Code witch highlight symbols
(defsubst el/slime-eval-string (str)
  (cadr (slime-eval `(swank:eval-and-grab-output ,str))))

;; Var with information for current region need to fontify 
(defvar el/shigh-live--*list-to-fontify* nil
  "do not change this var manualy.")

;; Function to prepare variable el/shigh-live--*list-to-fontify*
;; each time when font-lock-fontify-<someting> called
(defun el/shigh-live--jit-lock-prepare (start &optional end)
  (setq el/shigh-live--*list-to-fontify* nil)
  (setq end (max (or end 0)
                 (window-end)))
  (setq start (min start
                   (window-start)))
  (let ((res nil))
    (save-excursion
      (goto-char start)
      (while (car (push (highligh-slime-search-symbol end) res)))
      (setq el/shigh-live--*list-to-fontify*
            (when (cdr res)
              (read (downcase
                     (el/slime-eval-string
                      (format "(cl-slime-live-highlight::cl/slime-extract-symbols '%S)"
                              (nreverse (cdr res)))))))) )))
(defvar highligh-slime-search-symbol-regexp "\\(?:\\s_\\|\\sw\\)+")

;; Function to extract symbols from text
(defun highligh-slime-search-symbol (_limit)
  (with-syntax-table lisp-mode-syntax-table ;; (syntax-table)
    (when (search-forward-regexp highligh-slime-search-symbol-regexp  _limit t)
      `(,(car (match-data 0))
         ,(cadr (match-data 0))
         ,(match-string-no-properties 0)))))

(defvar el/shigh-face nil)

(defun el/slime-live-highlight-type-to-face (data)
  (case (read data)
    ((variable) shigh-variables-face)
    ((special)  shigh-special-face)
    ((macro)    shigh-macros-face)
    ((function) shigh-functions-face)))

(defun el/slime-live-fontify-function (_limit)
  (setq el/shigh-face nil)
  (when el/shigh-live--*list-to-fontify*
    (setq el/shigh-face
          (el/slime-live-highlight-type-to-face (caddr (car el/shigh-live--*list-to-fontify*))))
    (set-match-data
     `(,(car (car el/shigh-live--*list-to-fontify*))
        ,(cadr (car el/shigh-live--*list-to-fontify*))))
    (goto-char
     (cadr (car el/shigh-live--*list-to-fontify*)))
    (pop el/shigh-live--*list-to-fontify*)
    t))

(defun get-el/shigh-face ()
  el/shigh-face)

(defun slime-eval-print-last-expression-1 (string)
  "Evaluate sexp before point; print value into the current buffer.

It FIX: bag witch appear because slime-last-expression use slime-eval-print just after insertion."
  (interactive (list (slime-last-expression)))
  (lexical-let ((print-point (set-marker (make-marker) (point))))
    (slime-eval-async `(swank:eval-and-grab-output ,string)
      (lambda (result)
        (cl-destructuring-bind (output value) result
          (goto-char print-point)
          (push-mark)
          (insert "\n")
          (insert output value))))))

(advice-add 'slime-eval-print-last-expression :override #'slime-eval-print-last-expression-1)

(provide el-slime-live-highlight)
