;;; -*- mode: lisp; package: logging -*-
;;;
;;; logging.l --- ÉçÉOÇÈ
;;;
;;; Author:     bowbow99  <bowbow99@gmail.com>
;;; Version:    0.00.00
;;; Category:
;;; License:    MIT (see COPYING.mit)
;;; Commentary: (see README.md)
;;; ChangeLog:  (see ChangeLog)

#| [Draft] Usage

Put following in your code

    (defparameter *my-logger* (make-logger ...))
    
    (logging::setup *my-logger* ...)

then, you can use following macros to log information.

- @debug
- @info
- @msg
- @warn
- @error
- @fatal

If first argument to the macros is a literal string, then it is used as
format-control string for log entry and remaining arguments are used as
format-arguments.

    (@info "Doing something with `~S'" stuff)

    ;; log will be something like this
    20XX-08-21 03:02:01 [ INFO] Doing something with `#S(stuff ...)'

Alternatively, you can put any number of arbitrary expressions to log
their value.

    (@debug foo bar (hoge x y) (fuga x y))

    ;; log will be something like this
    20XX-01-02 03:03:03 [DEBUG] foo => "foo"
                                bar => "bar"
                                (hoge x y) => 3
                                (fuga x y) => t

Disabling logging
-----------------
If you set `*my-logger*` (or whatever variable you passed to `logging::setup`)
to `nil`, then logging macros will be eliminated.

Or you can set `(log-level *my-logger*)` to least important "entry-level" that
you want to log, then macro calls those are less important that you specified
will be eliminated.

NOTE: "eliminated" means that those macro-calls are expands into nil. So if you
configure to eliminate unnecessary logging macro-calls at compile time, then
you gen (almost) no overhead at run-time.

|#

#| TODOs

- Interface functions (or macros) to log
- clear-log?
- multiple logger use one buffer
- global logger
- align?
- filtering?
  current implementation doesn't print log-entries those are less important
  than -threshold. Instead of omit printing completely, store all log-entries
  somewhere and let user filter by entry-level later in log-buffer.

|#
;;; Code:

(eval-when (:execute :compile-toplevel)
  (require "typecase")
  (require "symbol-macrolet")
  (require "cmu_loop"))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "assertions"))


;;;;
;;;; * Package


(defpackage :logging
  (:use :lisp :editor))

(in-package :logging)

(export '(new-logger
          setup
          set-log-level))


;;;;
;;;; * Configs

(unless (boundp 'defcustom)
  ;; Let's pretend we have customization feature.

  (defmacro defgroup (name members &optional doc &rest options)
    nil)

  (defmacro defcustom (name default &optional doc &rest options)
    `(defparameter ,name ,default ,doc))

  (defmacro defcustom-local (name default &optional doc &rest options)
    `(defvar-local ,name ,default ,doc))

  (mapc (lambda (sym)
          (setf (get sym 'ed:lisp-indent-hook) 2))
        '(defgroup
          defcustom
          defcustom-local)))


;;; * logging
(defgroup logging nil  "Logging facility.")

(defcustom user::*logging-entry-level-threshold-default* :msg
  "*Default level of log-entry to be logged."
  :group logging
  :type (member :debug :info :msg :warn :error :fatal))

(defcustom user::*logging-entry-timestamp-format-default* "%Y-%m-%d %H:%M:%S"
  "*Default date-format string for timestamp, or `nil` for no timestamp."
  :group logging
  :type (or date-format-string nil))

(defcustom user::*logging-global-log-buffer-name* "*Log: ALL*"
  "*Buffer-name for 'global' log."
  :group logging
  :type string)


;;; * log-mode
(defgroup log-mode nil "See logs in buffer with highlight etc.")

(defcustom user::*log-mode-entry-timestamp-color* :comment
  "*Color of timestamp in log-buffer."
  :group log-mode
  :type keyword-color)

(defcustom user::*log-mode-entry-level-color-alist*
    '((:debug . (:color 5))
      (:info  . (:color 4))
      (:msg   . (:color 3))
      (:warn  . (:color 2))
      (:error . (:color 1 0 :bold))
      (:fatal . (:color 0 0 :bold)))
  "*Color of entry level in log-buffer."
  :group log-mode
  :type keyword-color)



;;;;
;;;; * Utilities

(defmacro with-switch-to-buffer (buffer &body body)
  `(with-set-buffer
     (let ((#1=#:obuffer (selected-buffer)))
       (unwind-protect
           (progn
             (set-buffer ,buffer)
             ,@body)
         (unless (eql #1# (selected-buffer))
           (set-buffer #1#))))))
#+xyzzy
(setf (get 'with-switch-to-buffer 'ed:lisp-indent-hook) 1)

(defmacro with-struct ((struct &rest slots) object &body body)
  (declare (type symbol struct)
           (type (list-of symbol) slots))
  (let ((package (symbol-package struct)))
    (labels ((accessor (slot)
               (let ((name (format nil "~A-~A" struct slot)))
                 (or (find-symbol name package)
                     (error "Accessor `~A' not found in package `~A'"
                            name (package-name package))))))
      `(let ((,struct ,object))
         (check-type ,struct ,struct)
         (symbol-macrolet
             (,@(mapcar (lambda (slot)
                          `(,slot (,(accessor slot) ,struct)))
                  slots))
           ,@body)))))
#+xyzzy
(setf (get 'with-struct 'ed:lisp-indent-hook) 2)

(defmacro with-ca/dr ((car-var cdr-var) cons &body body)
  `(let ((#1=#:cons ,cons))
     (declare (type cons #1#))
     (let ((,car-var (car #1#))
           (,cdr-var (cdr #1#)))
       ,@body)))
#+xyzzy
(setf (get 'with-ca/dr 'ed:lisp-indent-hook) 2)

;;;;
;;;; * Variables

(defparameter +entry-level-list+
  '(:debug :info :msg :warn :error :fatal))


;;;;
;;;; * Types

(deftype entry-level (&optional level)
  (if (eql level '*)
    `(member ,@+entry-level-list+)
    (progn
      (assert (member level +entry-level-list+))
      `(member ,@(member level +entry-level-list+)))))

(defstruct logger
  (destination nil ; (:buffer . "BUFFER-NAME") for now
    :type (cons keyword t))
  (threshold user::*logging-entry-level-threshold-default*
    :type entry-level)
  (timestamp-format user::*logging-entry-timestamp-format-default*
    :type string)
  )

(defun new-logger (buffer-name &key threshold timestamp)
  (declare (type string buffer-name)
           (type entry-level threshold)
           (type string timestamp))
  (make-logger 
   :destination (list :buffer buffer-name)
   :threshold
   (or threshold user::*logging-entry-level-threshold-default*)
   :timestamp-format
   (or timestamp user::*logging-entry-timestamp-format-default*)))

(defun set-log-level (logger level)
  (declare (type logger logger)
           (type entry-level level))
  (setf (logger-threshold logger) level))



;;;;
;;;; * Write Log Entry

(defun get-log-buffer (logger)
  (declare (type logger logger))
  (let* ((buffer-name (second (logger-destination logger)))
         (buffer (find-buffer buffer-name)))
    (unless buffer
      (setf buffer (get-buffer-create buffer-name)))
    (with-switch-to-buffer buffer
      (unless (eql buffer-mode 'log-mode)
        (log-mode logger)))
    buffer))

(defun write-log (logger level fmt-control &rest fmt-arguments)
  (declare (type logger logger)
           (type entry-level level)
           (type string fmt-control))
  (with-struct (logger threshold timestamp-format) logger
    (when (member level (member threshold +entry-level-list+))
      (let ((buffer (get-log-buffer logger)))
        (declare (type buffer buffer))
        (with-output-to-buffer (buffer (buffer-size buffer))
          (fresh-line)
          (when timestamp-format
            (format-date *standard-output* timestamp-format))
          (format *standard-output* " [~:@(~5@A~)] " level)
          (let* ((entry (apply #'format nil fmt-control fmt-arguments))
                 (lines (split-string entry #\LFD))
                 (col   (si:*stream-column *standard-output*)))
            (princ (car lines))
            (loop for line in (cdr lines)
              do (format t "~%~VT~A" col line))))))))


(defmacro setup (logger &key (prefix "@") (suffix "") (package *package*))
  (declare (type symbol logger)
           (type string prefix suffix))
  (labels ((gen-writer-name (level)
             (intern (format nil "~A~A~A" prefix level suffix)
                     package))
           (gen-defun-writer (level)
             (let ((name (gen-writer-name level)))
               `(defmacro ,name (datum &rest args)
                  (when (member ,level
                                (member (logger-threshold ,logger) +entry-level-list+))
                    (if (stringp datum)
                      `(write-log ,',logger ,',level ,datum ,@args)
                      (let ((forms (cons datum args)))
                        `(write-log ,',logger ,',level
                                    ,(with-output-to-string (out)
                                       (dolist (form forms)
                                         (format out "~S => ~~S~%" form)))
                                    ,@forms))))))))
    `(progn
       ,@(mapcar #'gen-defun-writer +entry-level-list+)
       t)))

;;;;
;;;; * log-mode

(defun make-timestamp-regexp (date-format)
  (labels ((char->regexp (c &optional pound-p)
             (case c
               ((#\d #\e #\H #\I #\m #\M #\S #\y)
                (if pound-p "[0-9]\\{1,2\\}" "[0-9]\\{2\\}"))
               (#\a "\\(?:Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\)")
               (#\A "\\(?:Monday\\|Tuesday\\|Wednesday\\|Thursday\\|Friday\\|Saturday\\|Sunday\\)")
               (#\b "\\(?:Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)")
               (#\B "\\(?:January\\|Feburary\\|March\\|April\\|May\\|June\\|July\\|September\\|October\\|November\\|December\\)")
               (#\E "\\(?:å≥\\|[0-9]\\{1,2\\}\\)")
               (#\g "\\(?:ñæé°?\\|ëÂê≥?\\|è∫òa?\\|ïΩê¨?\\)")
               (#\G "[MTSH]")
               (#\i "[0-9]\\{3\\}")
               (#\p "åﬂ[ëOå„]")
               (#\P (if pound-p "[ap]m" "[AP]M"))
               (#\v "[åéâŒêÖñÿã‡ìyì˙]")
               (#\Y "[0-9]\\{4\\}")
               (#\z "JST-9")
               (#\Z (if pound-p "\\+09:00" "+0900")))))
    (let ((len (length date-format))
          i)
      (while (setf i (string-match "%" date-format))
        (setf date-format
              (if (= (1+ i) len)
                (subseq date-format 0 i)
                (with-output-to-string (#1=#:out)
                  (unless (zerop i)
                    (princ (subseq date-format 0 i) #1#))
                  (multiple-value-bind (c pound-p)
                      (let ((next (char date-format (1+ i))))
                        (cond ((char/= next #\#) (values next nil))
                              ((= (+ i 2) len)   (values ""   nil))
                              (t                 (values (char date-format (+ i 2)) t))))
                    (princ (char->regexp c pound-p) #1#)
                    (let ((i (+ i (if pound-p 3 2))))
                      (unless (= i len)
                        (princ (subseq date-format i) #1#))))))))
      (concat "^" date-format))))

(defun make-entry-level-regexp (level)
  (declare (type entry-level level))
  (format nil "\\[\\(~:@(~5@A~)\\)\\]" level))

(defun make-log-mode-regexp-keyword-list (logger)
  (declare (type logger logger))
  (let ((timestamp-regexp
          (make-timestamp-regexp (logger-timestamp-format logger))))
    (compile-regexp-keyword-list
     `((,timestamp-regexp t ,user::*log-mode-entry-timestamp-color*)
       ,@(mapcar (lambda (x)
                   (with-ca/dr (level color) x
                     `(,(make-entry-level-regexp level) t ,color t 1 1)))
           user::*log-mode-entry-level-color-alist*)))))

(defvar *log-mode-hook* nil "*Hook for log-mode.")

(defun log-mode (&optional (logger nil))
  (interactive)
  (kill-all-local-variables)
  (setf mode-name   "Log"
        buffer-mode 'log-mode)
  (setup-temp-buffer (selected-buffer))
  (use-syntax-table ed::*lisp-mode-syntax-table*)
  (macrolet ((setq-local (var value &rest more)
               `(progn
                  (unless (local-variable-p ',var)
                    (make-local-variable ',var))
                  (setf ,var ,value)
                  ,@(when more
                      `((setq-local ,@more))))))
    (setq-local
     regexp-keyword-list (make-log-mode-regexp-keyword-list logger)
     keyword-hash-table  ed::*lisp-keyword-hash-table*))
  (run-hooks '*log-mode-hook*))

(provide "logging")
;;; logging.l ends here.
