# hanky-lisp
 
Hanky-lisp is some Common Lisp codes for daily programming.

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

