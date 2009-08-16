;; variable abclidea:*lisp-dir* is bound at the plugin
;; startup to a string representing absolute path, with
;; trailing slash, to  the driectory "lisp" of the 
;; plugin distribution. 

(load (concatenate 'string abclidea:*lisp-dir* "yanking.lisp"))

(load "C:/usr/unpacked/lisp-libs/slime/swank-loader.lisp")
(swank-loader:init)
(setf swank:*use-dedicated-output-stream* nil)
(swank:create-server :coding-system "utf-8-unix" :port 7777)

