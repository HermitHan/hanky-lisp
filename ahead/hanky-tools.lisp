;;;;hky-tools 是一部分实用工具的集合
(require "asdf")

;;;compose，将所有 funs 全部拼起来
(defun compose (&rest fns)
  (lambda (x)
    (reduce (lambda (v f) (funcall f v))
            fns
            :initial-value x)))
'(docs "like pipeline | in shell")
'(example ((funcall (compose #'1+ #'exp) 1)
           (funcall (compose #'exp #'1+) 1)))

(defun hello-a (a)
  (format t "Hello, ~A!~%" a))
'(docs "一个对 A 说 Hello 的测试程序")
'(keys (hello test))
'(example ((hello-a 'hky)))

;;;;0. reload-tools :: 重(编译)读取文件工具
;;返回一个可以重(编译)读取 file 的函数
;;可以通过 :new-file 来重置重读的文件位置
(defun make-compile-load (file)
  #'(lambda (&key (new-file nil) (if-compile nil))
      (if new-file
          (setf file new-file)
          (if if-compile
              (progn (compile-file file)
                     (load file))
              (load file)))))
'(docs "返回读取 file 的一个函数")
'(keys (file compile load))
'(example ((make-compile-load "./test.lisp")))

;;定义 with-all-gensyms 宏
(defmacro with-all-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
          syms)
     ,@body))
'(docs "十分重要的宏工具, 防止出现变量捕获, 接受需要 gensym 的符号与对应 body")
'(keys (gensym))
'(example ((with-all-gensyms (x y) (print x) (print y))))

;;通过宏定义两个函数, 分别是编译与不编译的重读函数
(defmacro defreload (reload-name filename
                     &optional (if-load-tools-default t))
  (with-all-gensyms (if-load-tools)
    `(defun ,reload-name (&optional (,if-load-tools
                                     ,if-load-tools-default))
       (load ,filename)
       (if ,if-load-tools
           (load-tools ,filename))
       (format nil "~A" ,filename))))
'(docs "可以用 reload-name 定义一个用于重读取 filename 文件的函数")
'(keys (reload load))
'(example ((defreload re-hanky-ahead
               (mkstr *hanky-lisp-location*
                      "ahead/hanky-tools.lisp"))))

(defmacro make-reload (file &optional
                              (name-list
                               (list 'reload 'creload)))
  (with-all-gensyms (reload-function)
    `(let ((,reload-function (make-compile-load ,file)))
       (defun ,(first name-list)
           (&key (new-file nil) (if-compile nil))
         (funcall ,reload-function :new-file new-file
                                   :if-compile if-compile))
       (defun ,(second name-list)
           (&key (new-file nil) (if-compile t))
         (funcall ,reload-function :new-file new-file
                                   :if-compile if-compile)))))
'(docs "可以构建两个用于重(编译)读取文件的函数")
'(keys (reload load compile))


;;;;1. list-tools :: 列表操作工具

;;;(1.1)短小需要内联编译的函数
(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))
'(docs "不同于 last, 取最后一个元素的 car, 而不是 cdr")
'(keys (list last))
'(example ((last1 '(1 2 3 4 5 4 3))))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))
'(docs "判断是否是一个单元素列表")
'(keys (list))
'(example ((single '(1)) (single '(1 2))))

(defun append1 (lst obj)
  (append lst (list obj)))
'(docs "在列表末尾 append 一个元素, 而非一个列表")
'(keys (list append))
'(example ((append1 '(a b) 'c)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))
'(docs "类似 append1, nconc 一个元素, 而非一个列表")
'(keys (list nconc))
'(example ((let ((s '(a b)))
             (print s)
             (conc1 s 'c)
             (print s))))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))
'(docs "确保 obj 是一个列表")
'(keys (list))
'(example ((mklist 'a)
           (mklist '(a b))))


;;;(1.2)一些较大的函数
(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))
'(docs "判断 x 的长度是否比 y 更长, 对列表进行了优化, 比 length 更优化")
'(keys (list length))
'(example ((longer '(1 2 3 4) '(5 6))
           (longer '(1 2 3) '(4 5 6))
           (longer '(1 2) '(3 4 5 6))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))
'(docs "使用 fn 方法过滤 lst 列表, 仅保留处理后非 nil 项")
'(keys (list))
'(example ((filter #'evenp '(1 2 3 4 5 6))
           (filter #'(lambda (x) (and (evenp x) (* x x)))
            '(1 2 3 4 5 6))))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))
'(docs "对 source 每 n 个进行一次分组, 形成列表的列表, 对 n = 0 时进行了判断, 防止无限递归")
'(keys (list))
'(example ((group '(a 1 b 2 c 3) 2)
           (group '(a 1 b 2 c 3) 3)
           (group '(a 1 b 2 c 3) 4)
           (group '(a 1 b 2 c 3) 10)))


;;;(1.3)使用双递归的列表实用工具
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))
'(docs "与 group 相反, 将一个列表压平. 接受一个带嵌套的列表, 将其压平")
'(keys (list))
'(example ((flatten '(1 (2 3) (4 (5 (6)))))))

(defun prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))
'(docs "remove-if 的向下递归版, 类似于 copy-tree/list. 对 tree 的每一项进行 test, 删除匹配项")
'(keys (list remove))
'(example ((prune #'evenp '(1 (2 3) (4 (5 6))))
           (prune #'consp '(1 (2 3) (4 5)))))


;;;(1.4)搜索列表的函数
(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))
'(docs "从 lst 中找到第一个 fn 作用后非 nil 的元素, 返回多值, 该元素与该元素调用 fn 后的值")
'(keys (list find))
'(example ((find2 #'1+ '(-1 1 -1 1 -1 -1))
           (find2 #'evenp '(1 3 4 7 6))))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))
'(docs "判断在 lst 中 x 元素是否在 y 元素之前")
'(keys (list))
'(example ((before 1 2 '(1 3 2 4))
           (before 1 2 '(0 1 2 3))
           (before 'a 'b '(a c))
           (before 'a 'b '(b c))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))
'(docs "与 before 相反, 判断 x 是否在 y 之后")
'(keys (list))
'(example ((after 'a 'b '(b a d))
           (after 'a 'b '(a))))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))
'(docs "判断 lst 中是否存在重复的 obj 元素, 并返回第二个的 cdr")
'(keys (list))
'(example ((duplicate 'a '(a n s a b c))
           (duplicate 'a '(a b c d))))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))
'(docs "按照某种规则分割列表, 第一个满足 fn 者开始为第二个返回值")
'(keys (list))
'(example ((split-if #'plusp '(0 -2 3 -1))
           (split-if #'plusp '(0 -2 -1))))


;;;(1.5)带有元素比较的搜索函数
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))
'(docs "对 lst 中每个元素应用 fn 打分并返回分数最高者")
'(keys (list find))
'(example ((most #'length '((a) (a b) (e f)))))

(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))
'(docs "对 lst 中每两个元素应用 fn 并返回最终优胜者")
'(keys (list find))
'(example ((best #'> '(1 2 3 4 5))))

(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))
'(docs "类似 most 但是返回 fn 打分后所有的最高分者, 而不是第一个")
'(keys (list find most))
'(example ((mostn #'length '(() (a) (e)))))


;;;(1.6)映射 map 类函数
(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))
'(docs "将 fn 映射到 a-b 的等差(步长 step)数列上")
'(keys (list map))
'(example ((mapa-b #'1+ -1 1 0.5)))

(defun map0-n (fn n)
  (mapa-b fn 0 n))
'(docs "对 0-n 的整数调用 fn 并串联成列表")
'(keys (list map))
'(example ((map0-n #'1+ 5)))

(defun map1-n (fn n)
  (mapa-b fn 1 n))
'(docs "对 1-n 的整数调用 fn 并串联成列表")
'(keys (list map))
'(example ((map1-n #'1+ 5)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))
'(docs "将 fn 映射到任意由后三个参数生成的数据结构上")
'(keys (list map))
'(example ((defun mapa-b1 (fn a b &optional (step 1))
             (map-> fn
                    a
                    #'(lambda (x) (> x b))
                    #'(lambda (x) (+ x step))))))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))
'(docs "提供了非破坏版的 mapcan, 用 append 代替 nconc")
'(keys (list map))
'(example ((mappend #'(lambda (x) (and (numberp x) (list x)))
            '(a 1 b c 3 4 d 5))))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))
'(docs "直接将 fn 映射到多个 list 中, 不同于 mapcar, 可直接应用多个列表")
'(keys (list map))
'(example ((mapcars #'sqrt '(1 4) '(9 16))))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))
'(docs "适用于树的 mapcar 版本, 即递归版 mapcar, 是 recursive mapcar 的缩写")
'(keys (list map))
'(example ((rmapcar #'princ '(1 (2 3 (4 (5)))))
           (rmapcar #'+ '(1 (2 (3))) '(4 (5 (6))))))



;;;;2. I/O-tools :: I/O 实用工具

;;;(2.1)三个实用 I/O 工具
(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                        (apply #'read-line args)
                        ")"))))
'(docs "读取一行并以列表格式返回 (行内进行 read 结果)")
'(keys (IO read))
'(example ((readlist)))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))
'(docs "读取并同时输出结果, 输出 args 并调用 readlist 并输出")
'(keys (IO read))
'(example ((prompt "Enter a number between ~A and ~A.~%>>"  1 10)))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.'~%")
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
          (return)
          (format *query-io* "~A~%" (funcall fn in))))))
'(docs "模拟 repl. 将 args 传给 prompt , 对结果传给 quit, nil 时传给 fn")
'(keys (IO repl read))
'(example ((break-loop #'eval #'(lambda (x) (eq x :q)) ">>")))



;;;;3. symbol/char/string-tools :: 操作符号/字符/字符串的工具

;;;(3.1)在字符/字符串/符号/列表之间相互转换的工具
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
'(docs "将 args 连在一起构建一个 string")
'(keys (symbol string))
'(example ((mkstr pi " pieces of " 'Pi)))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
'(docs "将 args 连在一起构建 string 然后转换成 symbol")
'(keys (symbol string))
'(example ((symb 'ar "Madi" #\L #\L 0)))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))
'(docs "类似 symb 但是返回任何 read 格式内容, 使用 read 而不是 intern 来构建符号")
'(keys (symbol string read))
'(example ((reread 'ar "Madi" #\L #\L 0)))

(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1
                                      :initial-element c)))
       (symbol-name sym)))
'(docs "接收一个 symbol 返回组成 symbol 的单个符号构成的列表")
'(keys (symbol list))
'(example ((explode 'hky)
           (explode '|Hky|)))


;;;(3.2)对字符串做搜索的工具
(defun smember (obj string &optional (ignore-case nil))
  (let ((obj? obj) (string? string))
    (if ignore-case
        (progn
          (setf obj? (string-upcase obj?))
          (setf string? (string-upcase string?))))
    (cond
      ((typep obj? 'character)
       (loop for i below (length string?) do
         (if (eql obj? (char string? i))
             (return-from smember (subseq string? i)))))
      ((typep obj? 'string)
       (let* ((len (length obj?))
              (up (- (length string?) len))
              (part nil))
         (loop for i to up do
           (setf part (subseq string? i (+ i len)))
           (if (equal obj? part)
               (return-from smember (subseq string? i)))))))))
'(docs "判断 obj 是否是 string 的子序列, 是 string member 的缩写")
'(keys (string member find))
'(example ((smember #\a "bbabc")
           (smember #\s "abcde")
           (smember "abb" "bbabbcde")
           (smember "abc" "bbabbcde")))

(defun in (obj sets &key (test #'eql) (ignore-case t))
  (cond
    ((typep sets 'list) (member obj sets))
    ((typep sets 'string)
     (smember (string obj) sets ignore-case))
    ((typep sets 'symbol)
     (let ((out (smember (symbol-name obj)
                         (symbol-name sets)
                         ignore-case)))
       (if out (intern out))))
    ((typep sets 'hash-table)
     (maphash #'(lambda (x y)
                  (if (or (funcall test obj x)
                          (funcall test obj y))
                      (return-from in (list x y))))
              sets))))
'(docs "判断 obj 是否在 sets 集合内")
'(keys (list string hash find))
'(example ((in 2 '(1 2 3))
           (in 4 '(1 2 3))
           (in "abb" "ddaabbacab")
           (in "abc" "ddaabbacab")
           (in 'abb 'ddaabbacab)
           (in 'abc 'ddaabbacab)
           (let ((s (make-hash-table)))
             (setf (gethash 'a s) 1)
             (setf (gethash 'b s) 2)
             (setf (gethash 'c s) 3)
             (print (in 'b s))
             (print (in '3 s))
             (print (in 4 s)))))

(defun split (astring &optional (by #\Tab))
  (cond
    ((eql astring nil) nil)
    (by
     (let ((out nil) (temp ""))
       (do* ((i 0 (1+ i))
             (now (ignore-errors (char astring i))
                  (ignore-errors (char astring i))))
            ((null now) (push temp out))
         (if (equal now by)
             (progn
               (push temp out)
               (setf temp ""))
             (setf temp
                   (concatenate
                    'string temp (string now)))))
       (nreverse out)))
    ((null by) astring)))
'(docs "从 string 中根据 by 字符分割 string 并返回 list")
'(keys (string split))
'(example ((split "chr1\	10000\	20000")))

(defun unite (alist &optional (by #\Tab))
  (if by
      (with-output-to-string (s)
        (princ (car alist) s)
        (dolist (i (cdr alist)) (princ by s) (princ i s)))
      alist))
'(example ((unite '(1 2 3 4))
           (unite '(A B #\Space))
           (unite '("chr1" 1234 2345 "chr1" 5678 6789))
           (unite "abc" nil)
           (unite '("chr1" 1234 2345 "chr1" 5678 6789) ",,")))

(defmacro unite1 (lst &optional (by #\Tab))
  `(format nil ,(mkstr "~{~A~^" by "~}") ,lst))
'(docs "将列表中的每个元素之间用 by 分割，生成一个字符串")
'(keys (string unite split))
'(example ((unite1 '(1 2 3 4))
           (unite1 '(A B #\Space))
           (unite1 '("chr1" 1234 2345 "chr1" 5678 6789))
           (unite1 '("abc") nil)))


;;;4、对 hash-table 哈希表进行操作的工具
(defun fn-test (obj fn test)
  (let ((out obj) (if-out t))
    (if fn (setf out (funcall fn obj)))
    (if test (setf if-out (funcall test out)))
    (if if-out out)))
'(docs "对 obj 先做 fn 然后 test, 若满足则返回 fn 后结果")
'(keys (test))
'(example ((fn-test 1 #'1+ #'oddp)
           (fn-test 1 #'1+ #'evenp)))

(defun hash-show (ahash &key (x-fn nil) (x-test nil)
                          (y-fn nil) (y-test nil)
                          (x-keep t) (y-keep t))
  (let ((out-keys nil))
    (maphash #'(lambda (x y)
                 (let ((xx (fn-test x x-fn x-test))
                       (yy (fn-test y y-fn y-test)))
                   (if (and xx yy)
                       (progn
                         (push x out-keys)
                         (if x-keep (setf xx x))
                         (if y-keep (setf yy y))
                         (format t "<~A>: ~S~%"
                                 xx yy)))))
             ahash)
    (nreverse out-keys)))
'(docs "从一个哈希表中打印所有的项, 可以对 y 内容应用 fn, 并由 test 测试是否输出, 返回所有输出值的 x")
'(keys (hash show))
'(example ((let ((atest (make-hash-table)))
             (setf (gethash 'a atest) 1)
             (setf (gethash 'b atest) 2)
             (setf (gethash 'c atest) 3)
             (hash-show atest :y-fn #'1+ :y-test #'evenp))))

(defun incf-hash (name ahash &optional (start 1))
  (if (gethash name ahash)
      (incf (gethash name ahash))
      (setf (gethash name ahash) start)))
'(docs "检测哈希表中是否有 name 键, 如果没有设为 start, 如果有则将其值 + 1")
'(keys (hash))
'(example ((let ((a (make-hash-table)))
             (incf-hash 'a a) (incf-hash 'a a)
             (incf-hash 'a a) (incf-hash 'b a)
             (incf-hash 'b a) (incf-hash 'c a)
             (incf-hash 'd a) (hash-show a))))

(defun hash-sort (ahash fn &key (x-or-y 'y)
                             (head (hash-table-size ahash)))
  (let ((alist nil))
    (maphash #'(lambda (x y) (push (list x y) alist)) ahash)
    (setf alist (sort alist
                      #'(lambda (x y)
                          (if (eql x-or-y 'x)
                              (funcall fn (car x) (car y))
                              (funcall fn (second x) (second y))))))
    (loop for i in alist and j below head do
      (format t "<~A>: ~S~%" (car i) (second i)))))
'(docs "对哈希表的数据进行排序并展出, 可以指定方法和 x 或 y")
'(keys (hash sort))
'(example ((let ((a (make-hash-table)))
             (incf-hash 'a a) (incf-hash 'a a)
             (incf-hash 'a a) (incf-hash 'b a)
             (incf-hash 'b a) (incf-hash 'c a)
             (incf-hash 'd a) (hash-sort a #'>))))

(defun hash-most (ahash fn &optional (x-or-y 'y))
  (let ((most nil))
    (maphash #'(lambda (x y)
                 (let ((most-x (car most))
                       (most-y (second most)))
                   (if most
                       (if (eql x-or-y 'x)
                           (when (funcall fn x most-x)
                             (setf most (list x y)))
                           (when (funcall fn y most-y)
                             (setf most (list x y))))
                       (setf most (list x y)))))
             ahash)
    most))
'(docs "对哈希表的数据两两进行 fn 测试并找到永远为真者")
'(keys (hash most))
'(example ((let ((a (make-hash-table)))
             (incf-hash 'a a) (incf-hash 'a a)
             (incf-hash 'a a) (incf-hash 'b a)
             (incf-hash 'b a) (incf-hash 'c a)
             (incf-hash 'd a) (hash-most a #'>))))


;;;5、字符串替换

;;环状缓冲区的操作
(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
  (svref (buf-vec buf)
         (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
               (mod n (length (buf-vec buf))))
        val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))

(defun buf-pop (b)
  (prog1
      (bref b (incf (buf-start b)))
    (setf (buf-used b) (buf-start b)
          (buf-new  b) (buf-end   b))))

(defun buf-next (b)
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-reset (b)
  (setf (buf-used b) (buf-start b)
        (buf-new  b) (buf-end   b)))

(defun buf-clear (b)
  (setf (buf-start b) -1 (buf-used  b) -1
        (buf-new   b) -1 (buf-end   b) -1))

(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))


;;字符串替换
(defun stream-subst (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((char= c (char old pos))
             (incf pos)
             (cond ((= pos len)             ;3
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)          ;2
                    (buf-insert c buf))))
            ((zerop pos)                    ;1
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t                              ;4
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))

(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
      (stream-subst old new in out))))
'(docs "可以从 file1 读取数据并将所有 old 字符串替换成 new, 并输入到 file2 文件")
'(keys (IO string substitution))


;;;6. not eql 系列函数, 节省输入 null 这一步
(defun neq (&rest body)
  (not (apply #'eq body)))
'(docs "判断是否 not eq")
'(keys (not eq))
'(example ((neq 1 2)
           (neq 1 1)
           (neq "a" "b")
           (neq "a" "a")))

(defun neql (&rest body)
  (not (apply #'eql body)))
'(docs "判断是否 not eql")
'(keys (not eq))
'(example ((neql 1 2)
           (neql 1 1)
           (neql "a" "b")
           (neql "a" "a")))

(defun nequal (&rest body)
  (not (apply #'equal body)))
'(docs "判断是否 not equal")
'(keys (not eq))
'(example ((nequal 1 2)
           (nequal 1 1)
           (nequal "a" "b")
           (nequal "a" "a")))


;;;;7. 读取文件相关
(defun read-to-list (file &optional (split nil))
  (let ((out nil))
    (with-open-file (in file)
      (do* ((line (read-line in nil) (read-line in nil)))
           ((null line) (nreverse out))
        (when split (setf line (split line split)))
        (push line out)))))
'(docs "从 file 逐行读取文件, 并考虑是否适用 split 分割每行")
'(keys (read list IO))

(defun write-list-to (alist file &key (split #\	)
                                   (direction :output)
                                   (if-exists :supersede)
                                   (if-does-not-exist :create))
  (with-open-file (out file :direction direction
                            :if-exists if-exists
                            :if-does-not-exist if-does-not-exist)
    (dolist (line alist)
      (let ((len-1 (1- (length line))))
        (dolist (i (subseq line 0 len-1))
          (format out "~A~A" i split))
        (format out "~A~%" (nth len-1 line))))))
'(docs "将一个矩阵列表写入 file, 并以 split 分割列")
'(keys (write list IO))



;;;;8、自制常用, 实用, 更强大的 linux 工具

;;;wc
(defun wc (file)
  (let ((num 1))
    (with-open-file (in file)
      (do ((line (read-line in nil) (read-line in nil)))
          ((null line) num)
        (incf num)))))
'(docs "统计一个 file 的行数")
'(keys (wc))

;;;head
(defun head (obj &key (n 10) (if-file t))
  (cond
    ((typep obj 'list)
     (loop for i in obj and j below n do
       (print i)))
    ((typep obj 'string)
     (if if-file
         (with-open-file (in obj)
           (do ((i (read-line in nil)
                   (read-line in nil))
                (j 1 (1+ j)))
               ((or
                 (null i)
                 (> j n)))
             (print i)))
         (print (subseq obj 0 n))))))
'(docs "取 obj 的前 n 个并 print 展示")
'(keys (head show))
'(example ((head '(1 2 3 4 5 6 7 8 9 10) :count 5)
           (head "abcdefghijklmn" :count 7 :if-file nil)))

;;;tail
(defun tail (obj &key (n 10) (if-file t))
  (let ((buf nil))
    (cond
      ((typep obj 'list)
       (loop for i in (reverse obj)
             and j below n do
               (push i buf))
       (format t "~{~A~%~}" buf))
      ((typep obj 'string)
       (if if-file
           (with-open-file (in obj)
             (do ((i (read-line in nil)
                     (read-line in nil)))
                 ((null i) (format t "~{~A~%~}" buf))
               (if (>= (length buf) n)
                   (progn
                     (pop buf)
                     (setf buf (append buf (list i))))
                   (setf buf (append buf (list i))))))
           (progn
             (setf buf (subseq (reverse obj) 0 n))
             (setf buf (nreverse buf))
             (format t "~A~%" buf)))))
    buf))
'(docs "取 obj 的后 n 个并 print 展示")
'(keys (head show))
'(example ((tail '(1 2 3 4 5 6 7 8 9 10) :count 5)
           (tail "abcdefghijklmn" :count 7 :if-file nil)))

;;;sort :: 因为可以同时 sort 多列, 所以取名 sorts
(defun sorts (alist &optional (sort-by nil))
  (let ((out (copy-tree alist)))
    (do* ((i 0 (1+ i))
          (info (cons (car sort-by) nil)
                (ignore-errors (subseq sort-by 0 (1+ i)))))
         ((null info))
      (setf out
            (sort out
                  #'(lambda (x y)
                      (let ((if? t))
                        (dolist (i info)
                          (let ((fn (second i)) (index (car i)))
                            (setf if? (and if?
                                           (funcall fn
                                                    (nth index x)
                                                    (nth index y))))))
                        if?)))))
    out))
'(docs "可以实现同时对多列执行依次排序")
'(keys (sort))
'(example ((sorts '((1 2 3) (1 2 4) (1 4 2) (1 3 4) (1 3 1) (1 4 1))
            '((0 <=) (1 <=) (2 <=)))
           (sorts '((1 2 3) (1 2 4) (1 4 2) (1 3 4) (1 3 1) (1 4 1))
            '((2 >=) (1 >=) (0 >=)))))

;;;string-number
(defmacro str-num<=> (<=>? x y)
  `(,<=>? ,(parse-integer x) ,(parse-integer y)))
'(docs "将两个字符串格式的数组，转换成整数并比较")
'(keys (string))
'(example ((str-num<=> < "1" "2")
           (str-num<=> <= "1" "2")
           (str-num<=> > "1" "2")
           (str-num<=> >= "1" "2")
           (str-num<=> = "1" "2")))

;;;make-str-num
(defmacro make-str-num<=> (<=>?)
  `(lambda (x y)
     (,<=>? (parse-integer x) (parse-integer y))))
'(docs "将两个字符串格式的数组，转换成整数并比较")
'(keys (string))
'(example ((funcall (make-str-num<=> <) "1" "2")
           (funcall (make-str-num<=> <=) "1" "2")
           (funcall (make-str-num<=> >) "1" "2")
           (funcall (make-str-num<=> >=) "1" "2")))

;;;sort-file-list :: 根据需求来排序某个文件
(defun sort-file-list
    (file &key (split #\	)
            (sort-by
             (list
              (list 0 #'string<=)
              (list 1 (make-str-num<=> <=))
              (list 2 (make-str-num<=> <=)))))
  (sorts (read-to-list file split) sort-by))
'(docs "从 file 文件中读取数据至列表，并返回根据 sort-by 要求排序后的列表")
'(keys (tad sort))

;;;combine-line :: 对两行进行判别，满足时合并
(defun combine-line (in-file out-file judge-fun out-fun
                     &optional (by #\Tab))
  (with-open-file (in in-file)
    (with-open-file (out out-file
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (do ((last-line nil this-line)
           (this-line (split (read-line in nil) by)
                      (split (read-line in nil) by)))
          ((equal (car this-line) "")
           (format out "~{~A~^\	~}~%" last-line))
        (if (ignore-errors
             (funcall judge-fun last-line this-line))
            (setf
             this-line
             (funcall out-fun last-line this-line))
            (when last-line
              (format out "~{~A~^\	~}~%" last-line)))))))
'(docs "对两行进行判别，满足时合并")
'(keys (combine file))

;;;case-test :: 更强大的 case
(defmacro case-test (keyform test &body keys)
  (let ((out (list 'cond)))
    (dolist (item keys)
      (push `((funcall ,test ,keyform ,(car item)) ,@(cdr item)) out))
    (nreverse out)))
'(docs "是加强版 case, 可以自定义 case 的 test 判断相等方法, 其余一样")
'(keys (test case))
'(example ((let ((dg "dGao") (hky #'string-equal))
             (case-test dg hky
               ("dg" (print 1) (print 2))
               ("dgao" (print dg) (print "dgao") (print hky))
               ("hky" (print 3) (print 4))))))

(defun all-null (alist)
  (dolist (i alist)
    (when i (return-from all-null nil)))
  t)
'(docs "判断一个列表中是否所有元素都是 nil")
'(keys (null))
'(example ((all-null '(1 nil 2 nil))
           (all-null '(nil nil nil))
           (all-null '(nil nil nil 0))))

(defmacro dolists ((var lists-list) &body body)
  (with-all-gensyms (temp-lines alist)
    `(do* ((nth-index 0 (1+ nth-index))
           (,var
            (let ((,temp-lines nil))
              (dolist (,alist ,lists-list)
                (push (car ,alist) ,temp-lines))
              (nreverse ,temp-lines))
            (let ((,temp-lines nil))
              (dolist (,alist ,lists-list)
                (push (nth nth-index ,alist) ,temp-lines))
              (nreverse ,temp-lines))))
          ((all-null ,var))
       ,@body)))
'(docs "对多个 list 同时进行循环，每次取出对应列表中第 i 个元素，若已经完结，则取 nil，直至所有列表都空，line-index 可以获取当前行数")
'(keys (do dolist))
'(example ((dolists (i (list (list 1 2)
                        (list 3 4 5 6)
                        (list 7 8 9)))
            (print nth-index)
            (print i))
           (dolists (i (list (list 1 2)
                        (list 3 4 5 6)
                        (list 7 8 9)))
            (print i)
            (print (ignore-errors (apply #'+ i))))))


;;; nth 相关
(defun nth-int (n list)
  (parse-integer (nth n list)))
'(docs "nth 后将对应的字符串转换成 int 整型")
'(keys (nth int))
'(example ((nth-int 0 '("1" "2" "3"))
           (nth-int 1 '("1" "2" "3"))
           (nth-int 2 '("1" "2" "3"))))

(defun nth-do (n list fun)
  (funcall fun (nth n list)))
'(docs "对 nth 的元素进行 fun")
'(keys (nth fun))
'(example ((nth-do 0 '(1 2 3) #'1+)
           (nth-do 1 '(1 2 3) #'exp)
           (nth-do 2 '(1 2 3) #'float)))

(defun get-number-splited (obj-string &optional (by #\.))
  (values (split obj-string by)))
'(docs "将任何形式的数值字符串变成 by 左右的两个整数")
'(keys (number trans))
'(example ((get-number-splited "1.2")
           (get-number-splited "1")
           (get-number-splited "3.2")))

(defun parse-number (obj)
  (if (typep obj 'string)
      (cond
        ((in "/" obj)
         (let ((obj-numbers
                 (get-number-splited obj #\/)))
           (reduce #'/
                   (mapcar #'parse-number
                           obj-numbers))))
        ((in "e" obj)
         (let ((obj-numbers
                 (get-number-splited obj #\e)))
           (float (* (parse-number (car obj-numbers))
                     (expt 10 (parse-integer
                               (second obj-numbers)))))))
        ((in "." obj)
         (let ((obj-numbers
                 (get-number-splited obj #\.)))
           (let ((if-plus (if (in "-" (car obj-numbers))
                              nil t)))
             (if if-plus
                 (+ (parse-integer (car obj-numbers))
                    (/ (parse-integer
                        (second obj-numbers))
                       (expt 10 (length
                                 (second obj-numbers))))
                    0.0)
                 (- (parse-integer (car obj-numbers))
                    (/ (parse-integer
                        (second obj-numbers))
                       (expt 10 (length
                                 (second obj-numbers))))
                    0.0)))))
        (t (parse-integer obj)))
      (if (typep obj 'number) obj)))
'(docs "将数字字符串转换为整数或有理数")
'(keys (number trans))
'(example ((parse-number "1")
           (parse-number 1)
           (parse-number "1.2")
           (parse-number "-1.2")
           (parse-number 1.2)
           (parse-number "123.456")
           (parse-number "2345e-2")
           (parse-number "-3.2345e-2")
           (parse-number "3/2")
           (parse-number "3/2/2")))

(defun nth-number (n list)
  (parse-number (nth n list)))
'(docs "nth 后将对应的字符串转换成 number")
'(keys (nth number))
'(example ((nth-number 0 '("1" "2.1" "3/2"))
           (nth-number 1 '("1" "2.1" "3/2"))
           (nth-number 2 '("1" "2.1" "3/2"))))

(defun nthes (n-list list &optional (nth-fun #'nth))
  (let ((out nil))
    (dolist (n n-list)
      (push (funcall nth-fun n list) out))
    (nreverse out)))
'(docs "可以给出不止一个 nth，而是输出多个 nth")
'(keys (nth))
'(example ((nthes '(0 3 2) '(a b c d e f g))
           (nthes '(0 3 3 2) '(a b c d e f g))
           (nthes '(1 3 5 4 2 6 0) '(a b c d e f g))
           (time (let ((a-big-list
                         (loop for i from 1 to (expt 11 6)
                               collect '(a b c d e f g))))
                   (last1 (mapcar
                           #'(lambda (x)
                               (nthes '(1 3 5 4 2 6 0) x))
                           a-big-list))))))

(defun get-nth (item list &optional (key #'eql))
  (labels ((index (alist n)
             (if (null alist)
                 nil
                 (if (funcall key item (car alist))
                     n
                     (index (cdr alist) (1+ n))))))
    (index list 0)))
'(docs "获取 item 在 list 中第一次在 key 下相等时的索引")
'(keys (nth index))
'(example ((get-nth 3 (list 0 1 2 3 4 5 6))
           (get-nth 3 (list 0 1 2 3 4 5 6 3 8))
           (get-nth 7 (list 0 1 2 3 4 5 6))))

(defun sort1 (sequence predicate &key key if-cdr)
  (if if-cdr
      (append (list (car sequence))
              (sort1 (cdr sequence) predicate
                     :key key :if-cdr nil))
      (sort (copy-list sequence) predicate :key key)))
'(docs "sort 的 copy-list 版本")
'(keys (sort copy-list))
'(example ((let ((a (list 1 2 3 4)))
             (format t "sorted: ~A~%" (sort1 a #'>))
             (format t "ori   : ~A~%" a))))

(defun sort2 (sequence predicate &key key if-cdr)
  (if if-cdr
      (append (list (car sequence))
              (sort1 (cdr sequence) predicate
                     :key key :if-cdr nil))
      (sort (copy-tree sequence) predicate :key key)))
'(docs "sort 的 copy-tree 版本")
'(keys (sort copy-tree))
'(example ((let ((a (list 1 2 3 4)))
             (format t "sorted: ~A~%" (sort2 a #'>))
             (format t "ori   : ~A~%" a))))




(defun clean-string (string
                     &optional (char-bag '(#\Space #\Tab)))
  (string-trim char-bag string))
'(docs "Clean the string's head and tail to make sure there are no more char-bag's chars")
'(example ((clean-string "  123  ")
           (clean-string "\t  abc \t  \t ")
           (clean-string "\txyt ")))

(defun null-string (string)
  (string= "" string))
'(docs "To test if the string is empty")
'(example ((null-string "123")
           (null-string "")
           (null-string " ")))

(defun not-null-string (string)
  (null (null-string string)))
'(docs "To test if the string is not empty")
'(example ((not-null-string "123")
           (not-null-string "")
           (not-null-string " ")))

(defun char0= (char string)
  (char= char (char string 0)))
'(docs "To test if the first char of the string is equal to the char")
'(example ((char0= #\: ":1")
           (ignore-errors (char0= #\: ""))
           (char0= #\: "1:")))

(defun cut-string-by (string by)
  (labels ((csb (before left by)
             (if (null-string left)
                 (list before)
                 (if (char0= by left)
                     (list before (subseq left 1))
                     (csb (mkstr before (char left 0))
                          (subseq left 1) by)))))
    (csb "" string by)))
'(docs "To cut the string by the by char")
'(example ((cut-string-by "123:xyz" #\:)
           (cut-string-by "123:" #\:)
           (cut-string-by ":xyz" #\:)
           (cut-string-by "123xyz" #\:)))

(defun get-now-path ()
  (let ((all-path (mkstr *load-truename*)))
    (mkstr
     (reverse (second (cut-string-by (reverse all-path) #\/)))
     #\/)))
'(docs "get now file's path(only folder)")

(defun maptree (function tree)
  (if (listp tree)
      (mapcar #'(lambda (x) (maptree function x)) tree)
      (funcall function tree)))
'(docs "map to a tree's all inner element")
'(keys (map tree))
'(example ((maptree #'1+
            (make-all-list (1 2 (3 (4 5) (6)) 7 8)))
           (maptree #'square
            (make-all-list (1 2 (3 (4 5) (6)) 7 8)))))




;;;;定义与 linux 命令相似的命令
(defun sh (command &key (output :interactive)
                     (error-output :interactive)
                     (ignore-error-status t))
  (uiop:run-program command
                    :output output
                    :error-output error-output
                    :ignore-error-status ignore-error-status))
'(docs "直接调用外部 shell 命令 command")
'(keys (shell linux sh))
'(example ((sh "ls ~/")
           (sh (mkstr "tree " *hanky-lisp-location*))
           (sh (mkstr "ls " *hanky-lisp-location*))))

(defun ls (&optional (path "./") (output :string))
  (sh (mkstr "ls " path) :output output))
'(docs "调用 ls 函数")
'(keys (sh ls))
'(example ((ls)))

(defun l (&optional (path "./") (output :string))
  (sh (mkstr "ls -CF " path) :output output))
'(docs "调用 l = ls -CF 函数")
'(keys (sh ls l))
'(example ((l)))

(defun ll (&optional (path "./") (output :string))
  (sh (mkstr "ls -alFh " path) :output output))
'(docs "调用 ll = ls -alFh 函数")
'(keys (sh ls ll))
'(example ((ll)))

(defun pwd (&optional (output :string))
  (if (eql output :string)
      (multiple-value-bind (path a b)
          (sh "pwd" :output output)
        (values (subseq path 0 (1- (length path)))
                a b))
      (sh "pwd" :output output)))
'(docs "调用 pwd 输出当前路径")
'(keys (sh pwd))
'(example ((pwd)))

(defun cd (&optional (path "~/"))
  (uiop:chdir path))
'(docs "调用 cd 函数")
'(keys (sh chdir direction cd))
'(example ((progn
             (pwd)
             (cd "./")
             (pwd))))

(defun load-file-by-read-fun (file read-fun)
  (with-open-file (in file)
    (labels ((lf (stream out)
               (let ((line (funcall read-fun stream)))
                 (if line
                     (lf stream (push line out))
                     (nreverse out)))))
      (lf in nil))))
'(docs "load from a bio file and make a bio file list with read-fun like #'read-line")
