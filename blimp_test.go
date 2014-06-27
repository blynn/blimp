package blimp

import (
  "os/exec"
  "strings"
  "testing"
)

func oneliner(t *testing.T, in, out string) {
  c := exec.Command("./blimp")
  c.Stdin = strings.NewReader(in)
  b, err := c.CombinedOutput()
  if err != nil {
    t.Fatal("input:", in, "runtime error:", err)
  }
  s := string(b)
  want := out + "\n";
  if s != want {
    t.Error("input:", in, "\nwant:", want, "got:", s);
  }
}

func TestBasic(t *testing.T) {
  oneliner(t, "(+ 1 2 3)", "6")
  oneliner(t, "(+ 1 (+ 2 (+ 3) (+)))", "6")
  oneliner(t, "(car '(1 2 3))", "1")
  oneliner(t, "(cdr '(1 2 3))", "(2 . (3 . nil))")
  oneliner(t, "(set 'third (lambda (x) (car (cdr (cdr x))))) (third '(1 2 3 4))", "[function]\n3")
  oneliner(t, "(let ((x 5)) (set 'add5 (lambda (y) (+ x y)))) (add5 123)", "[function]\n128")
  oneliner(t, "(defun f (x) (if (< x 1) 0 (+ x (f (+ x -1))))) (f 100)", "[function]\n5050")
}

// From http://clhs.lisp.se/Body/s_progn.htm
func TestProgn(t *testing.T) {
  oneliner(t, "(progn)", "nil")
  oneliner(t, "(progn 1 2 3)", "3")
  oneliner(t, `
(set 'a 1)
(if a
  (progn (set 'a nil) 'here)
  (progn (set 'a t) 'there))
a`, "1\nhere\nnil")
}

// From http://www.cse.chalmers.se/~rjmh/Papers/whyfp.html
func TestFP(t *testing.T) {
  oneliner(t, `(progn
(defun reduce (f a x) (if (null x) a (funcall f (car x) (reduce f a (cdr x)))))
(defun append (a b) (reduce cons b a))
(defun map(f x) (reduce (lambda (a b) (cons (f a) b)) nil x))
(reduce + 0 '(100 200 300))
)
(append '(1 2 3) '(4 5 6))
(map (lambda (x) (+ (+ x x) -1)) '(1 2 3 4 5))`,
`600
(1 . (2 . (3 . (4 . (5 . (6 . nil))))))
(1 . (3 . (5 . (7 . (9 . nil)))))`)
}

// http://www.paulgraham.com/rootsoflisp.html
func TestRootsOfLisp(t *testing.T) {
  oneliner(t, `(progn
(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cadar (x) (car (cdr (car x))))
(defun caddr (x) (car (cdr (cdr x))))
(defun caddar (x) (car (cdr (cdr (car x)))))

(defun null. (x)
  (eq x '()))

(defun and. (x y)
  (cond (x (cond (y 't) ('t '())))
        ('t '())))

(defun not. (x)
  (cond (x '())
        ('t 't)))

(defun append. (x y)
  (cond ((null. x) y)
        ('t (cons (car x) (append. (cdr x) y)))))

(defun list. (x y)
  (cons x (cons y '())))

(defun pair. (x y)
  (cond ((and. (null. x) (null. y)) '())
        ((and. (not. (atom x)) (not. (atom y)))
         (cons (list. (car x) (car y))
               (pair. (cdr x) (cdr y))))))

(defun assoc. (x y)
  (cond ((eq (caar y) x) (cadar y))
        ('t (assoc. x (cdr y)))))

(defun eval. (e a)
  (cond
    ((atom e) (assoc. e a))
    ((atom (car e))
     (cond
       ((eq (car e) 'quote) (cadr e))
       ((eq (car e) 'atom)  (atom   (eval. (cadr e) a)))
       ((eq (car e) 'eq)    (eq     (eval. (cadr e) a)
                                    (eval. (caddr e) a)))
       ((eq (car e) 'car)   (car    (eval. (cadr e) a)))
       ((eq (car e) 'cdr)   (cdr    (eval. (cadr e) a)))
       ((eq (car e) 'cons)  (cons   (eval. (cadr e) a)
                                    (eval. (caddr e) a)))
       ((eq (car e) 'cond)  (evcon. (cdr e) a))
       ('t (eval. (cons (assoc. (car e) a)
                        (cdr e))
                  a))))
    ((eq (caar e) 'label)
     (eval. (cons (caddar e) (cdr e))
            (cons (list. (cadar e) (car e)) a)))
    ((eq (caar e) 'lambda)
     (eval. (caddar e)
            (append. (pair. (cadar e) (evlis. (cdr e) a))
                     a)))))

(defun evcon. (c a)
  (cond ((eval. (caar c) a)
         (eval. (cadar c) a))
        ('t (evcon. (cdr c) a))))

(defun evlis. (m a)
  (cond ((null. m) '())
        ('t (cons (eval.  (car m) a)
                  (evlis. (cdr m) a)))))
t)

(eval. 'x '((x a) (y b)))
(eval. '(eq 'a 'a)  '())
(eval. '(cons x '(b c)) '((x a) (y b)))
(eval. '(cond ((atom x) 'atom)
              ('t 'list))
       '((x '(a b))))
(eval. '(f '(b c))
       '((f (lambda (x) (cons 'a x)))))
(eval. '((label firstatom (lambda (x)
                            (cond ((atom x) x)
                                  ('t (firstatom (car x))))))
         y)
       '((y ((a b) (c d)))))
(eval. '((lambda (x y) (cons x (cdr y))) 'a '(b c d)) '())`, `t
a
t
(a . (b . (c . nil)))
list
(a . (b . (c . nil)))
a
(a . (c . (d . nil)))`)
}
