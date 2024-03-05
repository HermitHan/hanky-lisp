# hanky-lisp
 
Hanky-lisp is some Common Lisp codes for daily programming, and it's writed based by sbcl on linux.

It's can be loaded simply just by load the "load-hanky-lisp.lisp". You can add a line in your .sbclrc:

    (load "[your-hanky-lisp-path]/load-hanky-lisp.lisp")

Then you can use the hanky-lisp! You can try to write (man 'man) in your repl.

It can do:

1. ahead: Some useful functions and macros for daily programming.
2. man: A package can control all codes starting with "def", like "defun", "defmacro", "defparameter" and even "defxxx" defuned by yourself.
   1. You need to write '(docs "xxx") and '(example ((...) (...) ...)) after your (defxxx ...) code.
   2. You need to use (load-tools "xxx.lisp") to load all tools in the file.
   3. You can use (find-tools 'xxx) to search tools with xxx.
   4. You can use (man 'tool-name) to see the details of the tool.
   5. You can use (example 'tool-name) to run the example of the tool(it's like testing).
   6. ...
3. mathlab: Some simple functions for math calculation.
4. filelab: Some simple functions for doing something to text files.
5. ...

