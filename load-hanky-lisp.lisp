(labels ((my-mkstr (&rest args)
           (with-output-to-string (s)
             (dolist (a args) (princ a s)))))
  (defparameter *hanky-lisp-location*
    (reverse (subseq (reverse (my-mkstr *load-truename*))
                     (length "load-hanky-lisp.lisp")))))
(defparameter *hanky-lisp-location* *hanky-lisp-location*)
'(docs "The root path of all hanky-lisp files")
'(example ((format nil "~A" *hanky-lisp-location*)))

(labels ((my-mkstr (&rest args)
           (with-output-to-string (s)
             (dolist (a args) (princ a s)))))
  (load (my-mkstr *hanky-lisp-location* "ahead/hanky-tools.lisp"))
  (load (my-mkstr *hanky-lisp-location* "man/man.lisp"))
  (defparameter *hanky-files*
    (my-mkstr *hanky-lisp-location* "hanky-files.txt" )))

(labels ((my-mkstr (&rest args)
           (with-output-to-string (s)
             (dolist (a args) (princ a s)))))
  (load-tools (my-mkstr *hanky-lisp-location*
                        "load-hanky-lisp.lisp")))

(labels ((my-mkstr (&rest args)
           (with-output-to-string (s)
             (dolist (a args) (princ a s))))
         (get-all-need-load-files
             (need-load-files-file folder-path)
           (with-open-file (in need-load-files-file)
             (let ((file-list nil))
               (do ((a-file (read-line in nil)
                            (read-line in nil)))
                   ((null a-file))
                 (push (my-mkstr folder-path a-file) file-list))
               (nreverse file-list)))))
  (defparameter *hanky-need-load-files*
    (get-all-need-load-files *hanky-files* *hanky-lisp-location*)))

(defmacro defload-file&tools (reload-name file-list
                              &optional (filename "DONE"))
  `(defun ,reload-name ()
     (dolist (file ,file-list)
       (load file))
     (dolist (file ,file-list)
       (load-tools file))
     (format nil "~A" ,filename)))
'(docs "定义一个 reload，从 file-list-file 读取其中所有文件，采用先 load，再 reload 这种方法，并且构造一个 reload-name 的函数来进行所有 load")
'(keys (load reload))
'(example ((defload-file&tools re-hanky
            *hanky-need-load-files* *hanky-files*)))

(defload-file&tools re-hanky
  *hanky-need-load-files* *hanky-files*)
'(docs "定义重新读取 hanky 的函数")
'(keys (load reload hanky))
'(example ((re-hanky)))

(re-hanky)
