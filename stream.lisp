;;;; stream.lisp

(in-package :docs-reference)


(defstruct task
  name
  (status :running)
  result
  error
  thread)

(defvar *tasks* nil)
(defvar *tasks-lock* (bt:make-lock "tasks"))

(defparameter *kernel-size* 4)


;;;; Background Processes


(defun call-in-background (thunk &key (name "task"))
  "Run THUNK in background thread. Returns a pollable TASK."
  (let ((task (make-task :name name))
	(out *standard-output*))
    (setf (task-thread task)
	  (bt:make-thread
	   (lambda ()
	     (let ((*standard-output* out))
	       (handler-case
		   (setf (task-result task) (funcall thunk)
			 (task-status task) :done)
		 (error (e)
		   (format t "Error: ~a~%" e)
		   (setf (task-error task) e
			 (task-status task) :error)))))
	   :name name))
    (bt:with-lock-held (*tasks-lock*) (push task *tasks*))
    task))

(defmacro in-background ((&key (name "task")) &body body)
  "Run BODY in a background thread, returns a pollable TASK."
  `(call-in-background (lambda () ,@body) :name ,name))

(defun tasks ()
  (bt:with-lock-held (*tasks-lock*)
    (dolist (task *tasks*)
      (format t "~A  ~A~@[  ERROR: ~A~]~%"
              (task-status task) (task-name task) (task-error task)))))

(defun await (task)
  "Blocks until TASK finishes."
  (bt:join-thread (task-thread task))
  (if (eq (task-status task) :error)
      (error (task-error task))
      (task-result task)))


;;;; Concurrency


(defun ensure-kernel (&optional (n *kernel-size*))
  "Create the shared lparallel kernel once; reuse thereafter."
  (unless lparallel:*kernel*
    (setf lparallel:*kernel* (lparallel:make-kernel n))))

(defun stop-kernel ()
  (when lparallel:*kernel*
    (lparallel:end-kernel)
    (setf lparallel:*kernel* nil)))
