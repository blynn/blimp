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
  oneliner(t, "(cdr '(1 2 3))", "(2 . (3 . NIL))")
  oneliner(t, "(set 'third (lambda (x) (car (cdr (cdr x))))) (third '(1 2 3 4))", "[lambda]\n3")
  oneliner(t, "(let ((x 5)) (set 'add5 (lambda (y) (+ x y)))) (add5 123)", "[lambda]\n128")
}
