

(defpackage cl-slime-live-highlight
  (:use :cl))

(in-package :cl-slime-live-highlight)

;; variable special macro function
(defun calc-highlight-from-list (lst)
  "`LST' is a list with struct ((p1 p2 string) ...)  and the function
return the list whose elements are ((p1 p2 FACE-TYPE)) where FACE-TYPE is an
element of the set \"variable\" \"special macro\" \"function\""
   (loop with res = nil
      for i in lst
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

;;; cl-slime-live-highlight ends here
