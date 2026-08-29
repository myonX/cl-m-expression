# M-Expression
M-Expression Parser for Common Lisp
## Usage
```lisp
;;;#M"" を有効化
(enable-m-expression-syntax)

#M"fact[n] = [eq[n;1] -> 1;T -> *[n;fact[-[n;1]]]]"
;fact

#M"fact[4]"
;24

;;;#M""を無効化
(disable-m-expression-syntax)
```
## Installation
available in ultralisp
```lisp
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
```
```lisp
(ql:quickload :m-expression)
```
