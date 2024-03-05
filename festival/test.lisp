(defreload re-test "~/.hanky-lisp/festival/test.lisp")

(defun clear-screen ()
  (format t "~c[2J" #\esc))

(defun move-cursor (x y)
  (format t "~c[~d;~dH" #\esc x y))

(defun draw-animation ()
  (clear-screen)
  (loop for i from 1 to 10 do
    (move-cursor i 1)
    (format t "O")
    (force-output)
    (sleep 0.1)))

(defun main ()
  (draw-animation))

(main)
