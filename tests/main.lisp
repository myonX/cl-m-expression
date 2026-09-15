(defpackage cl-m-expression/tests/main
  (:use :cl
        :cl-m-expression
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

(deftest parsar-test
         (testing "function call"
                  (ok (equal (parse-m-expression  "f[a;b;c]")
                             '(f a b c))))
         (testing "defun"
                  (ok (equal (parse-m-expression  "f[x;y] = g[a;b;c]")
                             '(defun f (x y) (g a b c)))))
         (testing "lambda"
                  (ok (equal (parse-m-expression  "lambda[[a;b;c];+[a;b;c]]")
                             '(lambda (a b c) (+ a b c)))))
         (testing "cond"
                  (ok (equal (parse-m-expression  "[p_1 -> e_1;p_2 -> e_2;p_n -> e_n]")
                             '(cond (p_1 e_1) (p_2 e_2) (p_n e_n)))))
         (testing "whole"
                  (ok (equal (parse-m-expression  "fact[n] = [eq[n;1] -> 1;T -> *[n;fact[-[n;1]]]]")
                             '(DEFUN FACT (N) (COND ((EQ N 1) 1) (T (* N (FACT (- N 1))))))))))
