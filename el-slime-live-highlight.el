
;;; Description:
;;; just scratch of way to highlighting
;;; Common Lisp symbols at real time.

;;; prefixes: slime-live-highlight-, shigh-

;;; WARN: during using this package Common Lisp variables
;;; { *, **, ***, +, ++, +++, /, //, /// } are not work right way
;;; (because slime-eval called each time when re-fontification)

;;; WARN: need the file cl-slime-live-highlight.lsp in the same dirrectory
;;; And hardly depends on it.

;; TODO:
;; 1. To add description.
;; 2. rename all functions and variables regularly way.
;; 3. add some customizing.
;; 4. delete commented codes.
;; 5. make highlighting according symbols packages

;; 6. add some tests about is all tools this package need
;; is available end wright code to generate informative
;; messages/errors otherwise.

(require 'font-lock)
(require 'jit-lock)
(require 'slime)
					; FIX: May be something else?
(eval-when-compile (require 'cl)) ; for cl-destructuring-bind, lexical-let

;; FIX: There is a bug that function
;; slime-eval-print-last-expression print resalt on the
;; wrong position of buffer (it's easy to fix, and already
;; fixed, by defined `:override' advice, but I don't know
;; exactly the nature of this bug.))

;;; Code:

;; FIX: I do not now the right way to load Common Lisp part using elisp
;; That's why I use next variable to store project dirrectory
(defvar slime-live-highlight--*project-dir*
  (or (and load-file-name
           (file-name-directory load-file-name))
      (and (buffer-file-name)
           (file-name-directory (buffer-file-name))))
  "The variable store path to dirrectory contain this package (`slime-live-highlight')")

;;; Faces
(defgroup Slime-Live-Highlight nil
  "Slime Powered Syntax Highlighting"
  :prefix "shigh-" :group 'matching :group 'font-lock :group 'programming)
(defface slime-live-highlight-macro-face
    '((t (:foreground "#00006DE06DE0"))) ; Like `font-lock-constant-face'.  Do not inherit.
  "Face used to highlight Lisp functions."
  :group 'Slime-Live-Highlight :group 'faces)
(defface slime-live-highlight-variables-face
    '((t (:foreground "Orchid"))) ; Like `font-lock-builtin-face'.  Do not inherit.
  "Face used to highlight Lisp variables."
  :group 'Slime-Live-Highlight :group 'faces)
(defvar shigh-variables-face 'slime-live-highlight-variables-face)
(defvar shigh-functions-face 'font-lock-function-name-face)
(defvar shigh-macros-face 'slime-live-highlight-macro-face)
(defvar shigh-special-face 'font-lock-keyword-face)
;;; Faces ends here

(defun shigh-slime-eval-string (str &optional log-it)
  (let ((slime-log-events log-it)) ; FIX: We need to do not print result of
                                ; slime-eval to any buffer, I am not sure that
                                ; this variable control it but it seems so (old-rat984)
    (cadr (slime-eval `(swank:eval-and-grab-output ,str)))))


;;; windows-scroll hook

;; FIX: To re-fontify fontification after each scrolling
;; looks not good but temporary may be it's ok.
(defun slime-live-highlight--scroll-function (window start)
  (cl-destructuring-bind (s e)
      (save-excursion
        (list (goto-char start)
              (line-beginning-position (window-size window))))
    (remove-text-properties s e '(fontified))))

(defun slime-live-highlight--add-scroll-functions ()
  (add-hook 'window-scroll-functions
            'slime-live-highlight--scroll-function
            nil
            t))

(defun slime-live-highlight--remove-scroll-functions ()
  (remove-hook 'window-scroll-functions
               'slime-live-highlight--scroll-function
               t))

;;; windows-scroll hook ends here

;;; "Parser" of lisp symbols
(defvar slime-live-highlight--search-symbol-regexp "\\(?:\\s_\\|\\sw\\)+")

(defun slime-live-highlight--search-symbol (_limit)
  ;; FIX: may be it should be more complicated
  ;; but as far as one can re-define syntax of Common Lisp
  ;; By using, for example,
  ;; set-macro-character and set-dispatch-macro-character function 
  ;; It's hard to invent something more useful to parse symbols
  ;; (As far as we have `#.' char-macro I would not like to parse symbols by
  ;; using Common lisp reader)
  (with-syntax-table lisp-mode-syntax-table 
    (when (search-forward-regexp slime-live-highlight--search-symbol-regexp  _limit t)
      `(,(car (match-data 0))
         ,(cadr (match-data 0))
         ,(match-string-no-properties 0)))))

;;; "Parser" ends here
 
(defvar slime-live-highlight--*list-to-fontify* nil
  "do not change this var manualy.")

(defun slime-live-highlight--jit-lock-prepare (start &optional end)
  (setq slime-live-highlight--*list-to-fontify* nil)
  (setq end (max (or end 0)
                 (window-end)))
  (setq start (min start
                   (window-start)))
  (let ((res nil))
    (save-excursion
      (goto-char start)
      (while (car
              (push
               (slime-live-highlight--search-symbol end)
               res)))
      (setq slime-live-highlight--*list-to-fontify*
            (when (cdr res)
              (read
               (downcase
                (shigh-slime-eval-string
                 (format "(cl-slime-live-highlight::calc-highlight-from-list '%S)"
                         (nreverse (cdr res)))))))) )))

(defvar slime-live-highlight--*shigh-face* nil)

(defun slime-live-highlight--fontify-function (_limit)
  (setq slime-live-highlight--*shigh-face* nil)
  (when slime-live-highlight--*list-to-fontify*
    (setq slime-live-highlight--*shigh-face*
          (slime-live-highlight--type-to-face
           (caddr (car slime-live-highlight--*list-to-fontify*))))
    (set-match-data
     `(,(car (car slime-live-highlight--*list-to-fontify*))
        ,(cadr (car slime-live-highlight--*list-to-fontify*))))
    (goto-char
     (cadr (car slime-live-highlight--*list-to-fontify*)))
    (pop slime-live-highlight--*list-to-fontify*)
    t))

(defun slime-live-highlight--type-to-face (data)
  (case (read data)
    ((variable) shigh-variables-face)
    ((special)  shigh-special-face)
    ((macro)    shigh-macros-face)
    ((function) shigh-functions-face)))

(defun slime-live-highlight--get-*shigh-face* ()
  slime-live-highlight--*shigh-face*)

(defun slime-live-highlight-eval-print-last-expression (string)
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

(advice-add 'slime-eval-print-last-expression
            :override
            #'slime-live-highlight-eval-print-last-expression)

(define-minor-mode slime-live-highlight-mode
                                        ; sorry I'am too lazy to write documentetion
                                        ; FIX: 
    "Yet no documentation! "
  :group 'Slime-Highlight
  (if slime-live-highlight-mode
      (condition-case -/er/-
          (progn
            (unless (slime-connected-p)
              (error "Need to connect to slime for slime-live-highlight-mode!"))
            (condition-case --error--
                (when (eq nil
                          (read
                           (downcase
                            (shigh-slime-eval-string
                             "(common-lisp::packagep (common-lisp::find-package :rat/slive-hlight))"
                             t))))
                  (shigh-slime-eval-string
                   (format
                    "(common-lisp::load %S)"
                    (concat
                     slime-live-highlight--*project-dir*
                     "cl-slime-live-highlight.lsp")) t))
              (error (message "%s" --error--)))
            (font-lock-add-keywords
             nil
             '((slime-live-highlight--fontify-function 0 (slime-live-highlight--get-*shigh-face*))) 'append)
            (jit-lock-register #'slime-live-highlight--jit-lock-prepare)
            (let ((p-min (point-min))
		  (p-max (point-max)))
              (slime-live-highlight--jit-lock-prepare p-min p-max)
              (comment (el/slime-live-fontify-region-force p-min p-max))
              (font-lock-flush))
            (slime-live-highlight--add-scroll-functions))
        (error (message "%s" -/er/-)))
    (font-lock-remove-keywords
     nil
     '((slime-live-highlight--fontify-function 0
        (slime-live-highlight--get-*shigh-face*))))
    (jit-lock-unregister #'slime-live-highlight--jit-lock-prepare)
    (slime-live-highlight--remove-scroll-functions)))

(provide 'slime-live-highlight)
;;; slime-live-highlight ends here
