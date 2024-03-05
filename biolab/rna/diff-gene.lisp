;;;;这是处理 rna 数据的包，目前主要是针对 fpkm 数据

(defun calculate-t&fc (group-1 group-2
                       &optional (if-zero 0.0001))
  (values (t-test-value group-1 group-2)
          (let* ((mean-1 (average group-1))
                 (mean-2 (average group-2)))
            (cond
              ;;;如果有出现 000 情况，先不要这行数据
              ((or (= mean-1 0) (= mean-2 0)) 1)
              ((and (= mean-1 0) (= mean-2 0)) 1)
              ((= mean-1 0) (/ mean-2 if-zero))
              ((= mean-2 0) (/ if-zero mean-1))
              (t (/ mean-2 mean-1))))))
'(docs "对一个列表，计算其 t 值与 fc 值并用双值返回")
'(keys (t-test p cut))
'(example ((let ((a (list 577.731694835897 391.484538270038))
                 (b (list 0.917303789149226 0.323049142432958
                          0.204525176111759 0.342091371916236)))
             (calculate-t&fc a b))
           (let ((a (list 1 3 2 4 3 5))
                 (b (list 2 1 5 4 2 4)))
             (calculate-t&fc a b))
           (let ((a (list 0 0))
                 (b (list 0 0 0 0)))
             (calculate-t&fc a b))))

(defun calculate-t&fc-from-a-list
    (rna-list group-1-nthes group-2-nthes
     &optional (nth-fun #'nth-number))
  (let ((group-1 (nthes group-1-nthes rna-list nth-fun))
        (group-2 (nthes group-2-nthes rna-list nth-fun)))
    (calculate-t&fc group-1 group-2)))
'(docs "对一个列表，计算其 t 值与 fc 值并用双值返回")
'(keys (t-test p cut))
'(example ((let ((a (list 577.731694835897 391.484538270038))
                 (b (list 0.917303789149226 0.323049142432958
                          0.204525176111759 0.342091371916236)))
             (calculate-t&fc-from-a-list
              (append a b) (list 0 1) (list 2 3 4 5) #'nth))
           (let ((a (list 1 3 2 4 3 5))
                 (b (list 2 1 5 4 2 4)))
             (calculate-t&fc-from-a-list
              (append a b) (list 0 1 2 3 4 5)
              (list 6 7 8 9 10 11) #'nth))
           (let ((a (list "0" "0"))
                 (b (list "0" "0" "0" "0")))
             (calculate-t&fc-from-a-list
              (append a b) (list 0 1) (list 2 3 4 5)
              #'nth-number))))

(defun add-t&fc-columns
    (rna-table group-1-nthes group-2-nthes
     &key (nth-fun #'nth-number) (if-cdr t))
  (if if-cdr
      (append (list (append (car rna-table)
                            (list "t-value" "fold-change")))
              (add-t&fc-columns (cdr rna-table)
                                group-1-nthes group-2-nthes
                                :nth-fun nth-fun
                                :if-cdr nil))
      (mapcar #'(lambda (x)
                  (append x
                          (multiple-value-list
                           (calculate-t&fc-from-a-list
                            x group-1-nthes group-2-nthes
                            nth-fun))))
              rna-table)))
'(docs "对一个表格添加 t 值与 fc 值的两列")
'(keys (t fc t-test))
'(example ((let ((rna-table
                   (columns
                    (list 1 2 3 4 5 6 7)
                    (load-table
                     (mkstr *hanky-lisp-location*
                            "biolab/rna/rna-fpkm.txt")))))
             (add-t&fc-columns rna-table
                               '(1 2) '(3 4 5 6)
                               :if-cdr t))
           (let ((rna-table
                   (columns
                    (list 1 2 3 4 5 6 7)
                    (load-table
                     (mkstr *hanky-lisp-location*
                            "biolab/rna/rna-fpkm.txt")))))
             (columns (list 0 7 8)
                      (add-t&fc-columns rna-table
                                        '(1 2) '(3 4 5 6)
                                        :if-cdr t)))))

(defun add-place-change-column
    (rna-table group-1-nthes group-2-nthes
     &key (nth-fun #'nth-number) (if-cdr t))
  (if if-cdr
      (append (list (append (car rna-table)
                            (list "place-change"
                                  "group1-place"
                                  "group2-place")))
              (add-place-change-column
               (cdr rna-table)
               group-1-nthes group-2-nthes
               :nth-fun nth-fun
               :if-cdr nil))
      (labels ((sort-by-fpkm (group-nthes)
                 (sort1 rna-table
                        #'(lambda (x y)
                            (> (average
                                (nthes group-nthes x
                                       nth-fun))
                               (average
                                (nthes group-nthes y
                                       nth-fun)))))))
        (let ((sort-1 (sort-by-fpkm group-1-nthes))
              (sort-2 (sort-by-fpkm group-2-nthes)))
          (mapcar #'(lambda (x)
                      (let ((nth-1
                              (get-nth x sort-1))
                            (nth-2
                              (get-nth x sort-2)))
                        (append
                         x (list (- nth-1 nth-2) nth-1 nth-2))))
                  rna-table)))))
'(docs "对 table 添加一列在 fpkm 均值表达下的变化情况，向前是正值，向后是负值")
'(keys (sort fpkm))
'(example ((let ((rna-table
                   (columns
                    (list 1 2 3 4 5 6 7)
                    (load-table
                     (mkstr *hanky-lisp-location*
                            "biolab/rna/rna-fpkm.txt")))))
             (columns (list 0 7 8 9)
                      (add-place-change-column
                       rna-table
                       '(1 2) '(3 4 5 6))))))

(defun add-t&fc&pc-columns
    (rna-table group-1-nthes group-2-nthes
     &key (nth-fun #'nth-number) (if-cdr t))
  (add-place-change-column
   (add-t&fc-columns rna-table group-1-nthes group-2-nthes
                     :nth-fun nth-fun :if-cdr if-cdr)
   group-1-nthes group-2-nthes
   :nth-fun nth-fun :if-cdr if-cdr))
'(docs "对 table 添加t-value，fold-change，place-change 这三类信息")
'(keys (sort fpkm t t-test fc ps fold-change place-change))
'(example ((let ((rna-table
                   (columns
                    (list 1 2 3 4 5 6 7)
                    (load-table
                     (mkstr *hanky-lisp-location*
                            "biolab/rna/rna-fpkm.txt")))))
             (head (columns (list 0 7 8 9 10 11)
                            (add-t&fc&pc-columns
                             rna-table
                             '(1 2) '(3 4 5 6)
                             :if-cdr t))))))

(defun diff-genes-by (rna-table
                      group-1-nthes group-2-nthes
                      t-nth fc-nth
                      &key (t-cut (get-p2t-cut
                                   (+ (length
                                       group-1-nthes)
                                      (length
                                       group-1-nthes)
                                      -2)
                                   0.01))
                        (fc-cut 2)
                        (nth-fun #'nth-number)
                        (if-cdr t))
  (if if-cdr
      (append (list (car rna-table))
              (diff-genes-by
               (cdr rna-table)
               group-1-nthes group-2-nthes
               t-nth fc-nth
               :t-cut t-cut
               :fc-cut fc-cut
               :nth-fun nth-fun
               :if-cdr nil))
      (remove-if-not
       #'(lambda (x)
           (let* ((t-value (abs (funcall nth-fun t-nth x)))
                  (fc-ori (funcall nth-fun fc-nth x))
                  (fc-value (if (> fc-ori 1)
                                fc-ori (/ fc-ori))))
             (and (> t-value t-cut)
                  (> fc-value fc-cut))))
       rna-table)))
'(docs "从 rna-table 识别差异基因")
'(keys (diff gene diff-gene t-test))
'(example ((let ((rna-table
                   (add-t&fc&pc-columns
                    (columns
                     (list 1 2 3 4 5 6 7)
                     (load-table
                      (mkstr *hanky-lisp-location*
                             "biolab/rna/rna-fpkm.txt")))
                    (list 1 2) (list 3 4 5 6))))
             (print rna-table)
             (columns (list 0 7 8 9)
                      (diff-genes-by
                       rna-table
                       '(1 2) '(3 4 5 6) 7 8
                       :fc-cut 5)))))
