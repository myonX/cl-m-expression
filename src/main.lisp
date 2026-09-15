(uiop:define-package cl-m-expression
                     (:use #:cl)
                     (:export :enable-m-expression-syntax
                              :disable-m-expression-syntax
                              :parse-m-expression)
                     (:nicknames :m-expr))
(in-package #:cl-m-expression)

;; blah blah blah.
;;;parser
(defun string->symbol (string)
  (intern (string-upcase string)))

(esrap:defrule spaces
               (* (or " "))
               (:lambda (list)
                        (declare (ignore list))))

(esrap:defrule decimal
               (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
               (:lambda (list)
                        (parse-integer (format nil "~{~A~}" list))))

(esrap:defrule number
               (or decimal))

(esrap:defrule symbol
               (+ (not (or " " "[" "]" ";" "=")))
               (:lambda (list)
                        (string->symbol (format nil "~{~A~}" list))))

(esrap:defrule atom
               (or number symbol))

(esrap:defrule expression
               (or defun cond lambda function-call atom))

(esrap:defrule function-call
               (and symbol "[" (esrap:? (and expression (* (and ";" expression)))) "]")
               (:lambda (list) (cons (car list) (parse-argument (third list)))))
;(:lambda (list) (cons (car (third list)) (cadr (third list))))

                                        ;λ[[a;b;c];a+b+c]
(defun parse-argument (sub-ast)
  (if (null sub-ast)
      ()
      `(,(car sub-ast) ,@(mapcar #'cadr (cadr sub-ast)))))

(esrap:defrule lambda
               (and "lambda" "[" "[" (esrap:? (and symbol (* (and ";" symbol)))) "]" ";" expression "]")
               (:lambda (list)
                        `(lambda ,(parse-argument (fourth list))
                                 ,(elt list 6))))

(esrap:defrule cond
               (and "[" expression spaces "->" spaces expression (* (and ";" expression spaces "->" spaces expression )) "]")
               (:lambda (list)
                        `(cond (,(second list) ,(elt list 5))
                               ,@(mapcar (lambda (e) (list (cadr e) (car (cddddr (cdr e))))) (elt list 6)))))

(esrap:defrule defun
               (and symbol "[" (esrap:? (and symbol (* (and ";" symbol)))) "]" spaces "=" spaces expression)
               (:lambda (list) `(defun ,(car list)
                                       ,(parse-argument (third list))
                                       ,(elt list 7))))

(defun parse-m-expression (m-str)
       (esrap:parse 'expression m-str))

;#M"fact[n] = [eq[n;1] -> 1;T -> *[n;fact[-[n;1]]]]"
(defun m-reader (stream char subchar)
  (declare (ignore char subchar))
  (let ((m-str (read stream t nil t)))
    (check-type m-str string)
    (parse-m-expression m-str)))

(defun enable-m-expression-syntax ()
  (set-dispatch-macro-character #\# #\M #'m-reader)
  t)

(defun disable-m-expression-syntax ()
  (set-dispatch-macro-character #\# #\M nil)
  t)
