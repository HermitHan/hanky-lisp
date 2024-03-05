;;;;这是一个用于构造各种数学计算工具的集合

;;使用了变量捕获 numbers，类似 loop 中的 it，因为 apply 对于列表有长度限制而 car 以后则没有该困扰
(defmacro defnumbers-fun
    (fun-name &body body)
  (with-all-gensyms (numbers-or-list)
    `(defun ,fun-name (&rest ,numbers-or-list)
       (let ((numbers (if (and (typep (car ,numbers-or-list)
                                      'list)
                               (single ,numbers-or-list))
                          (car ,numbers-or-list)
                          ,numbers-or-list)))
         ,@body))))
'(docs "定义接受可能是数字也可能是数字列表的函数，用numbers代替 &rest 参数")
'(keys (def numbers list))
'(example ((progn (defnumbers-fun sum (reduce '+ numbers))
                  (print (sum 1 2 3 4))
                  (print (sum '(1 2 3 4))))))

(defnumbers-fun sum (reduce '+ numbers))
'(docs "获取 numbers 的总和，接受一堆数字或一个数字列表")
'(keys (sum))
'(example ((sum 1 2 3 4 5 6 7)
           (sum 1 2 3)
           (apply #'sum
            (loop for i from 1 to 100 collect i))
           (sum
            (loop for i from 1 to 100 collect i))
           (sum
            (loop for i from 1 to 100000 collect i))
           (sum
            (loop for i from 1 to (expt 10 7) collect i))))


(defnumbers-fun average
  (labels ((avg (lst n a)
             (if (null lst)
                 (/ a n)
                 (avg (cdr lst) (1+ n) (+ a (car lst))))))
    (avg numbers 0 0)))
'(docs "获取 numbers 的平均值，接受一堆数字或一个数字列表")
'(keys (average))
'(example ((average 0 0 0 0)
           (average 1 2 3 4 5)
           (average 1.1 3 5 7 8.9)
           (average 1 2 3 4)
           (apply #'average
            (loop for i from 3 to 91 collect i))
           (average
            (loop for i from 3 to 91 collect i))
           (average
            (loop for i from 1 to 100001 collect i))
           (average
            (loop for i from 1 to (1+ (expt 10 7)) collect i))))

(defnumbers-fun median
  (multiple-value-bind (index if-left)
      (floor (length numbers) 2)
    (if (zerop if-left)
        (average (subseq numbers
                         (1- index) (1+ index)))
        (nth index numbers))))
'(docs "获取 numbers 的中位数，接受一堆数字或一个数字列表")
'(keys (median))
'(example ((median 0 1 3 5 7)
           (median 0 1 99 5 7)
           (median 1 3 5 7)
           (median 0 1 101 102)
           (median '(0 1 3 5 7))
           (median (loop for i from 1 to 10 collect i))
           (median (loop for i from 1 to (1+ (expt 10 7))
                         collect i))))

(defun /-0 (number &rest more-numbers)
  (cond ((= (car more-numbers) 0) "error")
        ((single more-numbers) (/ number
                                  (car more-numbers)))
        (t (apply #'/-0 (/ number (car more-numbers))
                  (cdr more-numbers)))))
'(docs "如果有 n/0 的情况，返回 \"error\"")
'(keys (/ 0))
'(example ((/-0 9 3)
           (/-0 0 0 3 2)
           (/-0 9 3 3)
           (/-0 9 3 0)
           (/-0 9 0 3)
           (/-0 9 3 3 3)))

(defun square (a-number)
  (* a-number a-number))
'(docs "计算一个数的平方")
'(keys (square))
'(example ((square 3)
           (square 3.3)
           (square 1.414)
           (square -2)))

(defnumbers-fun variance
  (let ((mean (average numbers))
        (len (length numbers)))
    (/ (sum (mapcar
             #'(lambda (x) (square (- x mean))) numbers))
       (- len 1))))
'(docs "计算一组数或一个列表的数的方差")
'(keys (variance))
'(example ((variance 0 1 3 5 7)
           (variance '(0 1 3 5 7))
           (variance 1 3 5 7)
           (variance 0 1 101 102)
           (variance 0 1 99 5 7)
           (variance (loop for i from 1 to 10 collect i))
           (variance (loop for i from 1 to (1+ (expt 10 7))
                           collect i))))

(defnumbers-fun standard-deviation
    (sqrt (variance numbers)))
'(docs "计算一组数据的标准差")
'(keys (variance standard-deviation))
'(example ((standard-deviation 0 1 3 5 7)
           (standard-deviation '(0 1 3 5 7))
           (standard-deviation
            (loop for i from 1 to 10 collect i))))


;;; T 检验
(defparameter *t-p-table*
  (mapcar #'(lambda (x)
              (mapcar #'parse-number x))
          (load-table
           (mkstr *hanky-lisp-location* "mathlab/t-p2.txt")
           :line-split #\,)))
'(docs "导入 t-p 表格")
'(keys (t-test p))
'(example (*t-p-table*))

(defun get-p2t-cut (df p-cut &optional (tail 2))
  (let ((p-cut-nth (get-nth p-cut (assoc 0 *t-p-table*))))
    (if p-cut-nth
        (if (= tail 1)
            (if (> p-cut-nth 1)
                (setf p-cut-nth (- p-cut-nth 1))
                (error "请检测您的 p-cut，太大了，请小一点。"))
            (unless (= tail 2)
              (error "请检查您的 tail，只支持 1 或 2。")))
        (error "请检测您的 p-cut，目前只支持从 ~A 中选择。"
               (assoc 0 *t-p-table*)))
    (if (member df (mapcar #'car *t-p-table*))
        (nth p-cut-nth (assoc df *t-p-table*))
        (error "请检查您的 df，目前仅支持从 ~A 中选择。"
               (mapcar #'car *t-p-table*)))))
'(docs "获取 p-cut 值对应的 t 值")
'(keys (t-test p))
'(example ((get-p-cut-t 4 0.05)
           (get-p-cut-t 4 0.05 1)))

(defun t-test-value (a-list b-list)
  (let* ((n-a (length a-list))
         (n-b (length b-list))
         (mean-a (average a-list))
         (mean-b (average b-list))
         (s2-a (variance a-list))
         (s2-b (variance b-list))
         (t-value (if (= (sqrt (+ (/ s2-a n-a)
                                  (/ s2-b n-b)))
                         0)
                      0
                      (/ (- mean-b mean-a)
                         (sqrt (+ (/ s2-a n-a)
                                  (/ s2-b n-b)))))))
    (values t-value s2-a s2-b)))
'(docs "计算 t 检验的 t 值(独立样本 t 检验)")
'(keys (t-test test))
'(example ((t-test-value (list 1 3)
            (list 11 13 14 11))
           (t-test-value (list 11 13 14 11) (list 1 3))
           (t-test-value (list 0 0) (list 0 0 0 0))))

(defun t-test (a-list b-list &key (p-cut 0.05) (tail 2))
  (let* ((df (+ (length a-list) (length b-list) -2))
         (t-value (t-test-value a-list b-list))
         (t-cut (get-p2t-cut df p-cut tail)))
    (if (> t-value t-cut)
        (values t-value t-cut)
        (values nil t-value t-cut))))
'(docs "计算 t 检验是否符合 p-cut 的显著性，符合返回 t-value 与 t-cut，不符合返回 nil 与 t-value 和 t-cut")
'(keys (t-test))
'(example ((let ((a (list 577.731694835897 391.484538270038))
                 (b (list 0.917303789149226 0.323049142432958
                          0.204525176111759 0.342091371916236)))
             (t-test a b :p-cut 0.05))
           (let ((a (list 1 3 2 4 3 5))
                 (b (list 2 1 5 4 2 4)))
             (t-test a b :p-cut 0.05))
           (let ((a (list 0 0))
                 (b (list 0 0 0 0)))
             (t-test a b :p-cut 0.01))))

;;;(LH.STATISTICS:T-TEST-TWO-SAMPLE 1 2 3 2 2 3)
;;;/home/kyhan/quicklisp/dists/quicklisp/software/statistics-20231021-git/lh-statistics.lisp
