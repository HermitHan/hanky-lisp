;;;;这是一个用来处理 file 的各种工具的汇总

;;;一、读取
(defun read-tab (afile &optional (split-by #\Tab))
  (let ((file-list nil))
    (with-open-file (table afile)
      (do* ((line (read-line table nil)
                  (read-line table nil))
            (line-list (split line split-by)
                       (split line split-by)))
           ((null line))
        (push line-list file-list)))
    (nreverse file-list)))
'(docs "将txt的表格数据读取至 split-by 分割的嵌套列表")
'(keys (read table))
'(example ((read-tab (mkstr *hanky-lisp-location*
                      "filelab/in-example.txt"))))


;;;二、取列
(defun cut-list (alist &rest columns)
  (let ((cut-list nil))
    (dolist (line-list alist)
      (let ((temp-cut-line nil))
        (dolist (column columns)
          (push (nth column line-list) temp-cut-line))
        (push (nreverse temp-cut-line) cut-list)))
    (nreverse cut-list)))
'(docs "对 list 进行 cut，取其中指定的列")
'(keys (cut))
'(example ((let ((file-list
                   (read-tab
                    (mkstr *hanky-lisp-location*
                           "filelab/in-example.txt"))))
             (cut-list file-list 0))
           (let ((file-list
                   (read-tab
                    (mkstr *hanky-lisp-location*
                           "filelab/in-example.txt"))))
             (cut-list file-list 0 1 2))
           (let ((file-list
                   (read-tab
                    (mkstr *hanky-lisp-location*
                           "filelab/in-example.txt"))))
             (cut-list file-list 3 1))))

;;;三、列表横向合并
(defun combine-lists (&rest lists-list)
  (let ((new-list nil))
    (dolists (lines lists-list)
      (push (apply #'append lines) new-list))
    (nreverse new-list)))
'(docs "可以将列表横向合并，最好是行数一致的列表，不然可能会出问题……")
'(keys (combine list))
'(example ((combine-lists
            '((1 2)
              (3 4))
            '((5 6)
              (7 8)))
           (combine-lists
            '((1 2)
              (3 4))
            '((1 2 3)
              (4 5 6)
              (7 8 9)))
           (combine-lists
            '((1 2 3)
              (4 5 6)
              (7 8 9))
            '((1 2)
              (3 4)))
           (combine-lists
            '((a11 a12)
              (a21 a22)
              (a31 a32))
            '((b11 b12)
              (b21 b22)
              (b31 b32))
            '((c11 c12)
              (c21 c22)
              (c31 c32)))))


;;;四、将列表输出为文件
(defun write-tab (a-tab-list out-file
                  &key (if-exists :supersede)
                    (if-does-not-exist :create)
                    (split-by #\Tab))
  (with-open-file
      (out out-file
           :direction :output
           :if-exists if-exists
           :if-does-not-exist if-does-not-exist)
    (dolist (line a-tab-list)
      (format out "~A~%" (unite line split-by)))))
'(docs "将 tab 的列表文件输出到 out-file，并用 TAB 分割")
'(keys (write tab))
'(example ((let ((alist
                   (read-tab
                    (mkstr *hanky-lisp-location*
                           "filelab/in-example.txt"))))
             (write-tab
              alist
              (mkstr *hanky-lisp-location*
                     "filelab/in-example.txt")))
           (let ((alist
                   (read-tab
                    (mkstr *hanky-lisp-location*
                           "filelab/in-example.txt"))))
             (write-tab
              alist
              (mkstr *hanky-lisp-location*
                     "filelab/in-example.txt")
              :if-exists :append
              :split-by ","))))
