(defreload re-tree "~/.hanky-lisp/festival/christma.lisp")

(defun clear-screen ()
  (format t "~c[2J" #\esc))

(defun set-cursor ()
  (format t "~c[H" #\esc))

(defun christmas-tree
    (radius &key (stream t)
              (random-times 8)
              (decorate
               (list "圣" "诞" "快" "乐")))
  (let ((decorate-len (length decorate)))
    (dotimes (i radius)
      (format stream " "))
    (format stream "*~%")
    (dotimes (level radius)
      (let ((space-number (- radius level 1)))
        (dotimes (i space-number)
          (format stream " "))
        (format stream "\/")
        (dotimes (i level)
          (if (= (random random-times) 0)
              (format stream "~A" (nth (random
                                        decorate-len)
                                       decorate))
              (format stream "||")))
        (format stream "\|\\~%")))))

;;;(make-tree *out-file* 38)
(clear-screen)
(dotimes (i 1000)
  (set-cursor)
  (christmas-tree 37)
  (sleep 0.3))
