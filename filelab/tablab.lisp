;;;;这是一个用来处理 table(txt 格式表格) 的各种工具的汇总
(defmacro deflet-fun (name let-thing-default &body body)
  (with-all-gensyms (new-it reset-it)
    `(let ((it ,let-thing-default)
           (,reset-it ,let-thing-default))
       ,@body
       (defun ,(symb name '-check) () it)
       (defun ,(symb name '-set) (,new-it)
         (setf it ,new-it))
       (defun ,(symb name '-reset) ()
         (setf it ,reset-it)))))
'(docs "定义一个包含 let 的函数，let 对应的参数用 it 指代，会包含本身，-check，-set，-reset 这几个设定 it 参数的函数。")
'(keys (defun def))


;;;一、read-xxx

;;1、一次只读一行
(deflet-fun read-line-except nil
  (defun read-line-except (&optional (stream *standard-input*)
                             (sb-impl::eof-error t)
                             eof-value recursive-p)
    (if it
        (multiple-value-bind (line left)
            (read-line stream sb-impl::eof-error
                       eof-value recursive-p)
          (when line
            (if (char= it (char line 0))
                (read-line-except stream sb-impl::eof-error
                                  eof-value recursive-p)
                (values line left))))
        (read-line stream sb-impl::eof-error
                   eof-value recursive-p))))
'(docs "忽略 it(nil，即不忽略) 开头的行，用 read-line-except-check 来查看忽略行的行首字符，用 read-line-except-set 来修改忽略行行首字符，用 read-line-except-reset 来重置忽略行行首字符")
'(keys (read read-line))
'(example ((with-open-file
               (in (mkstr *hanky-lisp-location*
                    "filelab/in-example.txt"))
             (read-line-except in))
           (read-line-except-check)
           (read-line-except-set #\*)
           (with-open-file
               (in (mkstr *hanky-lisp-location*
                    "filelab/in-example.txt"))
             (read-line-except in))
           (read-line-except-reset)
           (with-open-file
               (in (mkstr *hanky-lisp-location*
                    "filelab/in-example.txt"))
             (read-line-except in))))

(deflet-fun read-line2list #\Tab
  (defun read-line2list (&optional
                           (stream *standard-input*)
                           (sb-impl::eof-error t)
                           eof-value recursive-p)
    (split (read-line-except stream sb-impl::eof-error
                             eof-value recursive-p)
           it)))
'(docs "读将一行并用 it(Tab)(变量捕获) 分割，返回分割后的列表，用 read-line2list-check 查看分割字符，用 read-line2list-set 修改分割字符，用 read-line2list-reset 重置分隔符")
'(keys (read read-line read-line-split split))
'(example ((with-open-file
               (in (mkstr *hanky-lisp-location*
                    "filelab/in-example.txt"))
             (read-line2list in))
           (with-open-file
               (in (mkstr *hanky-lisp-location*
                    "filelab/in-example.txt"))
             (read-line-except-set #\*)
             (read-line2list in))
           (read-line-except-reset)
           (read-line2list-check)
           (read-line2list-set #\Space)
           (with-open-file
               (in (mkstr *hanky-lisp-location*
                    "filelab/in-example.txt"))
             (read-line2list in))
           (read-line2list-reset)))

;;2、一次读所有行
(defun read-lines-by (read-line-fun
                      &optional (stream *standard-input*)
                        (sb-impl::eof-error nil)
                        eof-value recursive-p)
  (let ((all-out nil))
    (do ((line (funcall read-line-fun
                        stream sb-impl::eof-error
                        eof-value recursive-p)
               (funcall read-line-fun
                        stream sb-impl::eof-error
                        eof-value recursive-p)))
        ((null line))
      (push line all-out))
    (nreverse all-out)))
'(docs "通过 read-line-fun 读取每一行，并将其推入一个列表")
'(keys (read read-line))
'(example ((with-open-file
               (in (mkstr *hanky-lisp-location*
                    "filelab/in-example.txt"))
             (read-lines-by #'read-line in))
           (with-open-file
               (in (mkstr *hanky-lisp-location*
                    "filelab/in-example.txt"))
             (read-lines-by #'read-line2list in))))

(defun read-lines-except (&optional (stream *standard-input*)
                            (sb-impl::eof-error nil)
                            eof-value recursive-p)
  (read-lines-by #'read-line-except stream
                 sb-impl::eof-error eof-value recursive-p))
'(docs "读取整个 stream 至一个列表，每行作为一个元素")
'(keys (read read-line read-line-except read-line2list))
'(example ((with-open-file
               (in (mkstr *hanky-lisp-location*
                    "filelab/in-example.txt"))
             (read-lines-except in))
           (read-line-except-set #\*)
           (with-open-file
               (in (mkstr *hanky-lisp-location*
                    "filelab/in-example.txt"))
             (read-lines-except in))
           (read-line-except-reset)))

(defun read-table (&optional (stream *standard-input*)
                     (sb-impl::eof-error nil)
                     eof-value recursive-p)
  (read-lines-by #'read-line2list stream
                 sb-impl::eof-error eof-value recursive-p))
'(docs "读取整个 stream 至一个双层列表(矩阵/表格)")
'(keys (read read-line read-line-except read-line2list table))
'(example ((with-open-file
               (in (mkstr *hanky-lisp-location*
                    "filelab/in-example.txt"))
             (read-table in))
           (read-line-except-set #\*)
           (with-open-file
               (in (mkstr *hanky-lisp-location*
                    "filelab/in-example.txt"))
             (read-table in))
           (read-line-except-reset)))


;;;二、load-xxx
(defun load-lines (filename &key (line-except nil)
                              (if-keep-set nil)
                              (element-type 'base-char)
                              (external-format :default)
                              (class 'sb-sys:fd-stream))
  (read-line-except-set line-except)
  (let ((all-lines
          (with-open-file
              (in-stream filename
                         :element-type element-type
                         :external-format external-format
                         :class class)
            (read-lines-except in-stream))))
    (unless if-keep-set
      (read-line-except-reset))
    all-lines))
'(docs "从文件读取 lines 并返回列表")
'(keys (read read-lines-except))
'(example ((load-lines (mkstr *hanky-lisp-location*
                        "filelab/in-example.txt"))
           (load-lines (mkstr *hanky-lisp-location*
                        "filelab/in-example.txt")
            :line-except #\*)
           (read-line-except-check)))

(defun load-table (filename &key (line-except #\#)
                              (line-split #\Tab)
                              (if-keep-set nil)
                              (element-type 'base-char)
                              (external-format :default)
                              (class 'sb-sys:fd-stream))
  (read-line-except-set line-except)
  (read-line2list-set line-split)
  (let ((table
          (with-open-file
              (in-stream filename
                         :element-type element-type
                         :external-format external-format
                         :class class)
            (read-table in-stream))))
    (unless if-keep-set
      (read-line-except-reset)
      (read-line2list-reset))
    table))
'(docs "从文件读取 table 并返回双层列表")
'(keys (read read-table table))
'(example ((load-table (mkstr *hanky-lisp-location*
                        "filelab/in-example.txt"))
           (load-table (mkstr *hanky-lisp-location*
                        "filelab/in-example.txt")
            :line-except #\*)
           (read-line-except-check)))


;;;三、write-xxx
(defun write-list (list stream &key (start 0) end)
  (dolist (line list)
    (write-line line stream :start start :end end)))
'(docs "将 list 中每个元素输出至 stream")
'(keys (write write-line list))
'(example ((let ((lines
                   (load-lines
                    (mkstr *hanky-lisp-location*
                           "filelab/in-example.txt"))))
             (write-list lines *standard-output*))))

(defun write-list-by (fun list stream &key (start 0) end)
  (if fun
      (dolist (line list)
        (write-list (funcall fun line) stream
                    :start start :end end))
      (write-list list stream :start start :end end)))
'(docs "将 list 中每个元素进行 fun，并输出至 stream")
'(keys (write write-line list))
'(example ((let ((lines
                   (load-lines
                    (mkstr *hanky-lisp-location*
                           "filelab/in-example.txt"))))
             (write-list-by #'reverse lines *standard-output*))))

(defun list2string (list &key (list-fun nil)
                           (split-by #\Tab)
                           (keep-indices nil))
  (let ((new-list (if list-fun
                      (funcall list-fun list)
                      list))
        (temp-list nil)
        (last-list nil))
    (if keep-indices
        (progn
          (dolist (index keep-indices)
            (push (nth index new-list) temp-list))
          (setf last-list (nreverse temp-list)))
        (setf last-list new-list))
    (unite last-list split-by)))
'(docs "将一个列表用 list-fun 处理后，用 split-by 分割，返回 keep-indices 中的列构成的 string")
'(keys (list string))
'(example ((list2string
            (load-lines
             (mkstr *hanky-lisp-location*
              "filelab/in-example.txt"))
            :list-fun #'reverse :split-by #\|)
           (list2string
            (load-lines
             (mkstr *hanky-lisp-location*
              "filelab/in-example.txt"))
            :list-fun #'print :split-by #\,
            :keep-indices '(0 7 1))))

(defun write-table (table stream &key (line-fun nil)
                                   (split-by #\Tab)
                                   (keep-indices nil)
                                   (start 0) end)
  (write-list (mapcar
               #'(lambda (line-list)
                   (list2string line-list
                                :list-fun line-fun
                                :split-by split-by
                                :keep-indices keep-indices))
               table)
              stream :start start :end end))
'(docs "将一个 table 写入 stream，其中每行用 line-fun 处理，然后用 split-by 分割，并保留此时 keep-indices 列，但是建议自行处理 table 列表后直接使用无参数的 write-table")
'(keys (write table))
'(example ((write-table
            (load-table (mkstr *hanky-lisp-location*
                         "filelab/in-example.txt")
             :line-except #\*)
            *standard-output*)
           (write-table
            (load-table (mkstr *hanky-lisp-location*
                         "filelab/in-example.txt")
             :line-except #\*)
            *standard-output*
            :line-fun #'reverse
            :split-by #\,)
           (labels ((add-s-a (x)
                      (let ((s-a (ignore-errors
                                  (/ (nth-int 3 x)
                                     (nth-int 2 x)))))
                        (append1 x (if s-a
                                       (float s-a)
                                       "score/age")))))
             (write-table
              (load-table (mkstr *hanky-lisp-location*
                                 "filelab/in-example.txt")
                          :line-except #\*)
              *standard-output*
              :line-fun #'add-s-a))
           (labels ((add-s-a (x)
                      (let ((s-a (ignore-errors
                                  (/ (nth-int 3 x)
                                     (nth-int 2 x)))))
                        (append1 x (if s-a
                                       (float s-a)
                                       "score/age")))))
             (write-table
              (load-table (mkstr *hanky-lisp-location*
                                 "filelab/in-example.txt")
                          :line-except #\*)
              *standard-output*
              :line-fun #'add-s-a
              :keep-indices '(0 1 4 3)))))


;;;四、save-xxx
(defun save-table (table filename
                   &key (line-fun nil)
                     (split-by #\Tab)
                     (keep-indices nil)
                     (start 0) end
                     (direction :output)
                     (element-type 'base-char)
                     (if-exists :error)
                     (if-does-not-exist :error)
                     (external-format :default)
                     (class 'sb-sys:fd-stream))
  (with-open-file (out filename
                       :direction direction
                       :element-type element-type
                       :if-exists if-exists
                       :if-does-not-exist if-does-not-exist
                       :external-format external-format
                       :class class)
    (write-table table out :line-fun line-fun
                           :split-by split-by
                           :keep-indices keep-indices
                           :start start :end end)))
'(docs "将一个 table 存到 filename 的文件中")
'(keys (save table write write-table))
'(example ((save-table (load-table
                        (mkstr *hanky-lisp-location*
                         "filelab/in-example.txt"))
            (mkstr *hanky-lisp-location* "filelab/try.txt"))))


;;;五、Table 处理
(defun columns (line-nthes table &optional (nth-fun #'nth))
  (mapcar #'(lambda (x) (nthes line-nthes x nth-fun)) table))
'(docs "从 table 中抽取 line-nthes 这些列")
'(keys (column table))
'(example ((columns '(1) (list (list 1 2 3)
                          (list 'a 'b 'c)
                          (list 'x 'y' z)
                          (list "A" "B" "C")))
           (columns '(2 0) (list (list 1 2 3)
                            (list 'a 'b 'c)
                            (list 'x 'y' z)
                            (list "A" "B" "C")))))
