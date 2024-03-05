;;;;这是一个关于实用函数的管理系统

;;;几种方法?
;;(1) 类 + 广义函数
;;(2) 结构体 + 函数
;;(3) 包 + (1)/(2)

;;构造列表包含所有的参数
(defparameter *tool-slots*
  '(name code args type docs keys example file))
'(docs "包含了所有的 tool 类的属性")
'(keys (tool))
'(example (*tool-slots*))

;;;;(一)、构造一个工具类
(defclass tool ()
  ;;工具名
  ((tool-name :initarg :name
              :initform (error "请给出工具名")
              :accessor tool-name
              :documentation "工具名称/symbol")
   ;;工具源码
   (tool-code :initarg :code
              :initform (error "请给出工具源码")
              :accessor tool-code
              :documentation "工具源码/list")
   ;;工具参数
   (tool-args :initarg :args
              :initform nil
              :accessor tool-args
              :documentation "工具参数/list")
   ;;工具类型
   (tool-type :initarg :type
              :initform 'tool
              :accessor tool-type
              :documentation "工具类型[fun/macro]/symbol")
   ;;工具说明
   (tool-docs :initarg :docs
              :initform nil
              :accessor tool-docs
              :documentation "工具说明/string")
   ;;工具关键词
   (tool-keys :initarg :keys
              :initform nil
              :accessor tool-keys
              :documentation "工具关键词/list")
   ;;工具示例
   (tool-example :initarg :example
                 :initform nil
                 :accessor tool-example
                 :documentation "工具示例/list")
   ;;工具文件
   (tool-file :initarg :file
              :initform nil
              :accessor tool-file
              :documentation "工具文件/string")))
'(docs "一个 tool 类, 包含一个实用工具的各种属性")
'(keys (tool))



;;;;(二)、构造哈希表来存储所有的 tool
(defvar *all-tools* (make-hash-table))
'(docs "一个包含了所有 tools 的 hash-table 哈希表")
'(keys (tool))
'(example (*all-tools*
           (progn (hash-show *all-tools*) nil)))



;;;;(三)、构造相关函数来管理 tools 工具

;;;(3.1)、针对单个 tool 的工具

;;;(1)、make-tool :: 使用 make-tool 来从源码自动构建工具
(defun get-def (def?)
  (let ((type? (intern (subseq (symbol-name def?) 3))))
    (if (eql type? 'un) (setf type? 'function))
    type?))
'(docs "从 defun/defmacro 等符号中获取 function/macro 等 type")
'(keys (def))
'(example ((get-def 'defun)
           (get-def 'defmacro)
           (get-def 'defclass)))

(defun get-info (code)
  (let ((name (second code))
        (args (third code))
        (type (get-def (first code))))
    (values name args type)))
'(docs "从 code 源代码中获取工具的 name, args 和 type")
'(example ((get-info '(defun hello (a)
                       (format t "Hello, ~A!" a)))))

;;make-tool
(defun make-tool (code
                  &key (docs nil) (keys nil)
                    (example nil) (hash *all-tools*)
                    (file nil) (if-eval t))
  (multiple-value-bind (name args type atool) (get-info code)
    (setf (gethash name hash)
          (setf atool
                (make-instance 'tool :name name
                                     :code code
                                     :args args
                                     :type type
                                     :keys keys
                                     :docs docs
                                     :example example
                                     :file file)))
    (if if-eval (eval code))
    atool))
'(docs "通过 code 来构造一个 tool 实例并将其以同名键推入 ahash")
'(keys (tool))
'(example ((make-tool '(defun hello-a (a)
                        (format t "Hello, ~A!~%" a))
            :docs "一个最经典的 Hello World 程序"
            :keys '(hello)
            :example '((hello-a 'hky)))))


;;;(2)、remove-tool :: 删除指定 tool
(defun remove-tool (tool-name &optional (ahash *all-tools*))
  (let ((the-tool (gethash tool-name ahash)))
    (if the-tool (let ((name (tool-name the-tool)))
                   (remhash tool-name ahash)
                   (format t "<~(~A~)> has been removed~3~~%"
                           name))
        (format t "Not Found Tool: ~A" tool-name))))
'(docs "从 ahash 中删除 tool-name 的工具")
'(keys (tool remove))
'(example (remove-tool 'hello-a *all-tools*))


;;;(3)、man :: 使用 man 宏来获取 tool 工具详细信息
(defun tool-? (alist)
  (let ((out nil))
    (loop for i in alist do
      (push (symb 'tool- i) out))
    (nreverse out)))
'(docs "可以把 alist 中每个元素前加上 tool- 前缀")
'(keys (tool))
'(example ((tool-? '(name type file))))

;;man
(defun man (tool-name &key (slots *tool-slots*)
                        (ahash *all-tools*))
  (let ((the-tool (gethash tool-name ahash))
        (args-list (tool-? slots)))
    (if the-tool
        (loop for i in args-list do
          (setf *print-case* :downcase)
          (format t "<~@(~A~)>: ~S~%"
                  (intern (subseq (symbol-name i) 5))
                  (funcall i the-tool))
          (setf *print-case* :upcase))
        (format t "Not Found Tool: <~@(~A~)>" tool-name))))
'(docs "获取 tool 工具的详细信息并打印至屏幕")
'(keys (tool))
'(example ((man 'hello-a)))


;;;(4)、example :: 展示某个 tool 的 example
(defun example (tool-name)
  (let* ((the-tool (gethash tool-name *all-tools*))
         (examples (tool-example the-tool)))
    (if examples
        (loop for i in examples do
          (format t "~%~50~~%>> ~(~S~)~%~%~{~S~^~%~}~%~50~~%"
                  i (multiple-value-list (eval i))))
        (format t "The <~@(~A~)> dose not have any example."
                (tool-name the-tool)))))
'(docs "展示某个 tool 的示例, 并以美观格式输出")
'(keys (tool))
'(example ((example 'hello-a)))


;;;(3.2)、关于 I/O 多个 tools 的读写工具

;;;(1)、load-tools :: 从一个文件读取全部函数并构建对应的 tool
(defun load-tools (filespec &key (ahash *all-tools*) if-eval)
  (with-open-file (tools filespec)
    (let ((code nil) (docs nil) (keys nil) (example nil))
      (do* ((temp (read tools nil) (read tools nil))
            (head (car temp) (car temp)))
           ((null temp) (if code
                            (make-tool
                             code :docs docs
                                  :keys keys
                                  :example example
                                  :hash ahash
                                  :file filespec
                                  :if-eval if-eval)))
        (if (eql 'quote head)
            (let ((i-type (car (second temp)))
                  (info (second (second temp))))
              (cond
                ((eql i-type 'docs) (setf docs info))
                ((eql i-type 'keys) (setf keys info))
                ((eql i-type 'example) (setf example info))))
            (if (eql 'def (intern
                           (subseq (symbol-name head) 0 3)))
                (when (typep (second temp) 'symbol)
                  (if code
                      (make-tool
                       code :docs docs
                            :keys keys
                            :example example
                            :hash ahash
                            :file filespec
                            :if-eval if-eval))
                  (setf code temp
                        docs nil
                        keys nil
                        example nil))
                (when (eql 'let
                           (intern
                            (subseq (symbol-name head) 0 3)))
                  (setf temp (append
                              (list 'deflet-fun
                                    (second (car (cddr temp)))
                                    (list nil))
                              (cddr temp)))
                  (if code
                      (make-tool
                       code :docs docs
                            :keys keys
                            :example example
                            :hash ahash
                            :file filespec
                            :if-eval if-eval))
                  (setf code temp
                        docs nil
                        keys nil
                        example nil)))))))
  ahash)
'(docs "从一个文件读取全部函数并构建对应的 tool")
'(key (load tool))
'(example (((load-tools "./hky-tools.lisp"))))

;;(load-tools "/home/ubuntu/program/lisp/tools/code/hky-tools.lisp" :if-eval nil)

;;;(2)、save-tools :: 把 tools 写入(append)某个文件
(defun new-items (old-hash new-hash)
  (let ((new-items (make-hash-table)))
    (maphash #'(lambda (x y)
                 (if (null (second
                            (multiple-value-list
                             (gethash x old-hash))))
                     (setf (gethash x new-items) y)))
             new-hash)
    new-items))
'(docs "对两个哈希表, 检查有哪些后者有前者没有的键, 返回具后者独有者的新哈希表")
'(keys (hash diff))
'(example ((let ((a (make-hash-table))
                 (b (make-hash-table)))
             (setf (gethash 'new b) 'nb)
             (new-items a b))))

(defun tool-outype (atool)
  (let ((result (cons (tool-code atool) nil))
        (docs (tool-docs atool))
        (keys (tool-keys atool))
        (example (tool-example atool)))
    (if docs (push `'(docs ,docs) result))
    (if keys (push `'(keys ,keys) result))
    (if example (push `'(example ,example) result))
    (nreverse result)))
'(docs "从一个 tool 实例输出存入文件的 '(docs ...) 格式列表")
'(keys (tool))
'(example ((tool-outype (gethash 'hello-a *all-tools*))))

(defun write-tool (atool &optional stream)
  (let ((out (tool-outype atool)))
    (loop for i in out do (setf *print-case* :downcase)
                          (pprint i stream)
                          (setf *print-case* :upcase))
    (format stream "~%")
    out))
'(docs "对一个 tool 实例将其以特殊格式输出到 stream 流")
'(keys (tool IO))
'(example ((write-tool (gethash 'hello-a *all-tools* t))))

;;save-tools
(defun save-tools (filespec &key (ahash *all-tools*)
                              (if-exists :append))
  (with-open-file (tools filespec
                         :if-exists if-exists
                         :if-does-not-exist :create))
  (let ((old-hash (make-hash-table)) (new-tools nil))
    (load-tools filespec :ahash old-hash :if-eval nil)
    (setf new-tools (new-items old-hash ahash))
    (with-open-file (tools filespec :direction :output
                                    :if-exists if-exists)
      (maphash #'(lambda (x y)
                   (format t "~@(~A~) ===> ~S~%"
                           x filespec)
                   (write-tool y tools))
               new-tools))))
'(docs "将 ahash 里的 tools 添加到 filespec 里, 只添加没有键的新 tools")
'(keys (tool IO))


;;;(3)、update-tools :: 更新 tools 的具体参数
;;需要创建一个 buffer 来存储前一个 tool 实现 supersede 更新
(defun cluster-item (item)
  (if (typep item 'cons)
      (let ((head (car item)) (name (second item)))
        (cond
          ((eql 'quote head) (values 'slot name))
          ((in 'def head) (values (get-def head) name))
          (t (values 'others head))))
      (values 'others item)))
'(docs "可以对 file 文件中每个 () 对应的 item 分类并返回多值")
'(keys (tool))
'(example ((cluster-item '(defun fun-name ()))
           (cluster-item '(defmacro mac-name ()))
           (cluster-item ''(docs "..."))
           (cluster-item '(hello-a 'hky))
           (cluster-item 'abc)
           (cluster-item *all-tools*)))

(defun update-tools (in-filespec
                     &key (ahash *all-tools*)
                       (out-filespec in-filespec))
  (let ((if-push t) (new-out nil))
    (with-open-file (in in-filespec)
      (do* ((item (read in nil) (read in nil)))
           ((null item))
        (let* ((item-info (multiple-value-list
                           (cluster-item item)))
               (item-type (car item-info))
               (item-name (second item-info)))
          (cond
            ((eql 'slot item-type)
             (if if-push (push item new-out)))
            ((eql 'others item-type)
             (push item new-out) (setf if-push t))
            (t (let ((tool (gethash item-name ahash)))
                 (if tool
                     (progn
                       (setf if-push nil)
                       (push tool new-out))
                     (progn
                       (setf if-push t)
                       (push item new-out)))))))))
    (with-open-file (out out-filespec
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (loop for i in (nreverse new-out) do
        (setf *print-case* :downcase)
        (if (typep i 'tool) (write-tool i out)
            (pprint i out))
        (setf *print-case* :upcase))))
  (format t "~A === UPDATE ===> ~A"
          in-filespec out-filespec))
'(docs "将当前 *all-tools*  与 filespec 比对并更新有变化的 tools, 不过建议修改文件然后 reload 而不是修改 tools 再 update")
'(keys (tool IO))



;;;(3.3)、关于批量搜索 tools 的搜索并展示工具

;;;(1)、show-tools :: 从 *all-tools* 中针打印符合要求的 tools
(defun show-tools (&key (ahash *all-tools*)
                     (x-fn nil) (x-test nil)
                     (y-fn #'tool-docs) (y-test nil)
                     (x-keep t) (y-keep nil))
  (hash-show ahash :x-fn x-fn :x-test x-test
                   :y-fn y-fn :y-test y-test
                   :x-keep x-keep :y-keep y-keep))
'(docs "打印 ahash 的每一项, 并可以选择输出 tool 的哪一属性")
'(keys (tool))
'(example ((show-tools :ahash *all-tools* :y-fn #'tool-keys
                       :y-test #'(lambda (x) (in 'tool x)))))


;;;(2)、find-tools :: 从 *all-tools* 中按照固定格式搜索指定内容
(defun strings (obj)
  (if obj
      (if (typep obj 'cons)
          (concatenate 'string
                       (strings (car obj))
                       (strings (cdr obj)))
          (concatenate 'string
                       ;;在出现逗号等特殊符号时 string 会报错
                       (ignore-errors (string obj)) " "))))
'(docs "将一系列参数转换成 string 格式并用空格分隔, 列表会递归")
'(keys (string list))
'(example ((strings '(a b (c d (e f)) "G H" "I"))))

(defun tool-info (tool slots)
  (let ((info nil) (args-list (tool-? slots)))
    (loop for i in slots and j in args-list do
      (push (list i (strings (funcall j tool))) info))
    (nreverse info)))
'(docs "从 tool 中提取 slots 中的参数对应的属性数据并转换成关键字")
'(keys (tool))
'(example ((tool-info (gethash 'hello-a *all-tools*)
            '(name code docs keys))))

;;find-tools
(defun find-tools (obj &key (ahash *all-tools*)
                         (slots *tool-slots*)
                         (no-slots nil)
                         (if-print t)
                         (if-return nil))
  (let ((out nil) (info nil) (temp nil)
        (find-slots (copy-list slots)))
    (if no-slots (mapcar #'(lambda (x)
                             (setf find-slots
                                   (remove x find-slots)))
                         no-slots))
    (maphash #'(lambda (x y)
                 (setf temp nil)
                 (setf info (tool-info y find-slots))
                 (loop for i in find-slots do
                   (if (in obj (second (assoc i info)))
                       (push i temp)))
                 (if temp
                     (push (list x (nreverse temp)) out)))
             ahash)
    (if if-print
        (progn
          (format t "~%<<===== ~@(~A~) =====>>~%" obj)
          (loop for i in (reverse out) do
            (format t "<~A>: ~{~@(~A~)~^ ~}~%"
                    (car i) (second i)))
          (format t "<<===== ~@(~A~) =====>>~%" obj)))
    (if if-return
        (nreverse out))))
'(docs "比 hash-show 更强大的, 从 ahash 中全局搜索匹配指定关键字的工具")
'(keys (tool find))
'(example ((find-tools '* :slots '(name))
           (find-tools '* :no-slots '(example code))))

;;;(3)、man-tools :: 将 find-tools 的数据美观输出
(defun man-tools (tools-list &key (ahash *all-tools*)
                               (man-all nil))
  (mapcar #'(lambda (x)
              (let* ((xx (copy-list x))
                     (temp (second xx)))
                (format t "~30~~%")
                (if man-all
                    (setf temp *tool-slots*)
                    (progn
                      (setf temp (remove 'name temp))
                      (setf temp (remove 'docs temp))
                      (pushnew 'docs temp)
                      (pushnew 'name temp)))
                (man (car xx)
                     :slots temp
                     :ahash ahash)
                (car x)))
          tools-list))
'(docs "通过 ((name (args*))*) 格式列表输出相应的 man 数据")
'(keys (tool))
'(example ((man-tools (find-tools 'strings))))


;;;(4)、man-find :: 将 find, man 结合, 直接 man find 的结果
(defun man-find (obj &key (ahash *all-tools*)
                       (slots *tool-slots*) (no-slots nil)
                       (if-print t) (man-all nil))
  (man-tools (find-tools obj :ahash ahash
                             :slots slots
                             :no-slots no-slots
                             :if-print if-print
                             :if-return t)
             :ahash ahash :man-all man-all))
'(docs "将 find 和 man 结合, 直接 man 出 find-tools 的结果")
'(keys (tool find))
'(example ((man-find '* :slots '(name) :man-all t)))


;;;;同时 load file 和 tools
(defun load-file-and-tools (file)
  (if (probe-file file)
      (progn (load file)
             (load-tools file))
      (error
       "Please check your file!~%File does not exist: ~A~%"
       file)))
'(docs "load file and load-tools of the file")
'(example ((load-file-and-tools "~/apps/HankyBioHub/HankyBioEngine/lisp/code/now-version/biohub-may-hanky-basic-funs/")))
