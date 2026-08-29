(uiop:define-package cl-m-expression
                     (:use #:cl)
                     (:export :enable-m-expression-syntax
                              :disable-m-expression-syntax))
(in-package #:cl-m-expression)

;; blah blah blah.
#|
(asdf:initialize-source-registry
  `(:source-registry
    (:directory #p"~/lisp/app/m-expression/")
:inherit-configuration))

(asdf:load-system :m-expression)
|#

;;;tools
(defun upcase-intern (string)
  (intern (string-upcase string )))

(defun parse-integer-p (str)
  (handler-case
      (progn
        (parse-integer str)
        t)                   ; パース成功なら t を返す
    (parse-error () nil)))

(defun except-first-and-last-element (list)
  (subseq list 1 (1- (length list))))

(defun predicate-checker (token-list)
  (format t "defun ~A~% lambda  ~A~% function call  ~A~% cond  ~A~% atom  ~A~%"
          (defun-p token-list)
          (lambda-p token-list)
          (function-call-p token-list)
          (cond-p token-list)
          (atomp token-list)))

#|
fact[n] = [eq[n;1] -> 1;T -> *[n;fact[-[n;1]]]]
f[a;b;c]
f[x;y] = x+y
[p_1 -> e_1;p_2 -> e_2;p_n -> e_n]
λ[[a;b;c];a+b+c]
|#

(defun removes (items sequence &key (test #'eql))
  (if items
      (removes (cdr items) (remove (car items) sequence :test test) :test test)
      sequence))

(defun tokenizer (string)
  (labels ((r-tokenizer (string ans check-index)
                        (cond ((or (< (1- (length string)) check-index)) (removes '("" " ") (reverse (cons string ans)) :test #'string=))
        ((position (elt string check-index) "[](); ")
         (r-tokenizer  (subseq string (1+ check-index) ) `(,(subseq string check-index (1+ check-index)) ,(subseq string 0 check-index) ,@ans) 0))
        (t (r-tokenizer string ans (1+ check-index))))))
          (r-tokenizer string () 0)))

(defun atomp (list);最初の要素を判定
  (cond ((= 1 (length list)) t)
        ((position (cadr list) '(";" "->") :test #'string=) t)
        (t ())))

(defun function-call-p (token-list)
  (cond ((atomp token-list) ())
        ((and (not (position (car token-list) '("[") :test #'string=))
              (position (cadr token-list) '("[") :test #'string=)) t)));後で書く

(defun cond-p (token-list)
  (if (string= (car token-list) "[")
      t
    nil))

(defun lambda-p (token-list)
  (string= (car token-list) "lambda"))

(defun defun-p (token-list)
  (and (not (position (car token-list) '("[" "->")))
       (string= "[" (cadr token-list))
       (string= "=" (elt token-list
                         (1+ (shift-begin-index #'position-correspond-]
                                                token-list
                                                1))))))

(defun position-correspond-] (token-list);最初の要素を判定
  (labels ((r (token-list depth n)
              (cond ((= n 0) (1- depth))
                    ((string= "[" (car token-list)) (r (cdr token-list) (1+ depth) (1+ n)))
                    ((string= "]" (car token-list)) (r (cdr token-list)  (1+ depth) (1- n)))
                    (t (r (cdr token-list)  (1+ depth) n)))))
          (r (cdr token-list) 1 1)))

(defun shift-begin-index (function token-list begin)
       (+ begin (funcall function (subseq token-list begin))))

(defun get-argument (list)
  (labels ((r (token-list &optional argument-list)
              (cond ((not token-list) (reverse argument-list))
                    ((atomp token-list) (r (cddr token-list)
                                           (cons (atom-parser token-list) argument-list)))
                    ((function-call-p token-list)
                     (r (subseq token-list (let ((]-position (1+ (position-correspond-] (cdr token-list)))))
                                             (if (string= ";" (elt token-list (1+ ]-position)))
                                                 (+ 2 ]-position)
                                                 (1+ ]-position))))
                        (cons (function-call-parser (subseq token-list 0 (+ 2 (position-correspond-] (cdr token-list))))) argument-list)))
                    )))
          (r (subseq list 1 (position-correspond-] list)))))

(defun function-call-parser (list)
  (declare (ignorable list))
  (if (string= (car list) "lambda")
      ()
      `(,(atom-parser list) ,@(get-argument (cdr list)))))

(defun lambda-parser (token-list)
  (let* ((end-arg-list (1+ (shift-begin-index #'position-correspond-] token-list 2)))
         (arg-list (get-argument (subseq token-list 2 end-arg-list)))
         (body (parser (subseq token-list (1+ end-arg-list) (1- (length token-list))))))
       `(lambda ,arg-list ,body)))

(defun defun-parser (token-list)
  (let  ((argument-end (1+ (shift-begin-index #'position-correspond-] token-list 1))))
    (multiple-value-bind (a a-end)
                         (parser (subseq token-list (1+ argument-end)))
                         (values `(defun ,(upcase-intern (car token-list))
                                         ,(get-argument (cdr token-list))
                                         ,a)
                                  (+ 2 argument-end a-end)))))

(defun cond-parser (token-list)
  (let* ((cond-phrase (subseq token-list 0 (1+ (position-correspond-] token-list)))))
        (labels ((r-cond-parser (token-list)
                                (if token-list
                                    (multiple-value-bind (p_1 p_1-end)
                                                         (parser token-list)
                                                         (multiple-value-bind (e_1 e_1-end)
                                                                              (parser (subseq token-list (1+ p_1-end)))
                                                                              (cons `(,p_1 ,e_1)
                                                                                    (r-cond-parser (if (< (length token-list) (+ 2 e_1-end p_1-end))
                                                                                                       ()
                                                                                                     (subseq token-list (+ 2 e_1-end p_1-end))))))))))
                (cons 'cond (r-cond-parser (except-first-and-last-element cond-phrase))))))

(defun atom-parser (token-list)
  (if (parse-integer-p (car token-list))
      (parse-integer (car token-list))
      (upcase-intern (car token-list))))

(defun parser (token-list)
  (cond ((defun-p token-list) (defun-parser token-list))
        ((lambda-p token-list) (values (lambda-parser token-list) (1+ (position-correspond-] (cdr token-list)))))
        ((function-call-p token-list) (values (function-call-parser token-list) (+ 2 (position-correspond-] (cdr token-list)))))
        ((cond-p token-list) (values (cond-parser token-list) (1+ (position-correspond-] token-list))))
        ((atomp token-list) (values (atom-parser token-list) 1))))

(defun m-expression-string->s-expression (string)
       (parser (tokenizer string)))

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
