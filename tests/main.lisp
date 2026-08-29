(defpackage cl-m-expression/tests/main
  (:use :cl
        :clm-expression
        :rove))
(in-package :cl-m-expression/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :m-expression)' in your Lisp.


#|
fact[n] = [eq[n;1] -> 1;T -> *[n;fact[-[n;1]]]]
[n] = [eq[n;1] -> 1;T -> *[n;fact[-[n;1]]]]"
f[a;b;c]
f[x;y] = x+y
[p_1 -> e_1;p_2 -> e_2;p_n -> e_n]
λ[[a;b;c];a+b+c]
|#


(deftest tokenizer-test
         (testing "function call"
                  (ok (equal '("f" "[" "a" ";" "b" ";" "c" "]")
                             (cl-m-expression::tokenizer "f[a;b;c]"))))
         
         (testing "function call"
                  (ok (equal '("f" "[" "x" ";" "y" "]" "=" "x+y")
                             (cl-m-expression::tokenizer "f[x;y] = x+y"))))
         
         (testing "function call"
                  (ok (equal '("[" "p_1" "->" "e_1" ";" "p_2" "->" "e_2" ";" "p_n" "->" "e_n" "]")
                             (cl-m-expression::tokenizer "[p_1 -> e_1;p_2 -> e_2;p_n -> e_n]"))))
         
         (testing "function call"
                  (ok (equal '("lambda" "[" "[" "a" ";" "b" ";" "c" "]" ";" "a+b+c" "]")
                             (cl-m-expression::tokenizer "lambda[[a;b;c];a+b+c]"))))
         
         (testing "function call"
                  (ok (equal '("fact" "[" "n" "]" "=" "[" "eq" "[" "n" ";" "1" "]" "->" "1" ";" "T" "->" "*" "[" "n" ";" "fact" "[" "-" "[" "n" ";" "1"
 "]" "]" "]" "]")
                             (cl-m-expression::tokenizer "fact[n] = [eq[n;1] -> 1;T -> *[n;fact[-[n;1]]]]")))))

(deftest parsar-test
         (testing "function call"
                  (ok (equal (cl-m-expression::m-expression-string->s-expression "f[a;b;c]")
                             '(f a b c))))
         (testing "defun"
                  (ok (equal (cl-m-expression::m-expression-string->s-expression "f[x;y] = g[a;b;c]")
                             '(defun f (x y) (g a b c)))))
         (testing "lambda"
                  (ok (equal (cl-m-expression::m-expression-string->s-expression "lambda[[a;b;c];+[a;b;c]]")
                             '(lambda (a b c) (+ a b c)))))
         (testing "cond"
                  (ok (equal (cl-m-expression::m-expression-string->s-expression "[p_1 -> e_1;p_2 -> e_2;p_n -> e_n]")
                             '(cond (p_1 e_1) (p_2 e_2) (p_n e_n)))))
         (testing "whole"
                  (ok (equal (cl-m-expression::m-expression-string->s-expression "fact[n] = [eq[n;1] -> 1;T -> *[n;fact[-[n;1]]]]")
                             '(DEFUN FACT (N) (COND ((EQ N 1) 1) (T (* N (FACT (- N 1))))))))))
