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
