(in-package :apex-utilities)

(declaim (optimize (debug 3)))

;;; Unicode utilities

(defparameter *char-to-unicode-table* (make-hash-table))
(defparameter *unicode-to-char-table* (make-hash-table))

(defun char-to-unicode (char)
  (or (gethash char *char-to-unicode-table*) 0))

(defun unicode-to-char (unicode)
  (or (gethash unicode *unicode-to-char-table*) #\_))

(defun set-char-unicode-correspondance (char unicode)
  (setf (gethash char *char-to-unicode-table*) unicode
	(gethash unicode *unicode-to-char-table*) char))

(loop for char in '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
		    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z)
      for code from 65
      do (set-char-unicode-correspondance char code))

(loop for char in '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
		    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)
      for code from 97
      do (set-char-unicode-correspondance char code))

(loop for char in '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
      for code from 48
      do (set-char-unicode-correspondance char code))

