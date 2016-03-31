(defun make-env ()
  '())

(defun env-first-frame (env)
  (car env))

(defun env-rest (env)
  (cdr env))

(defun env-extend (env vars vals)
  (cons (make-frame vars vals) env))

(defun env-get (env var)
  (if (null env)
      (error "undefined variable: ~a" var)
      (multiple-value-bind (val exist) (frame-get (env-first-frame env) var)
	(if (null exist)
	    (env-get (env-rest env) var)
	    val))))


(defun make-frame (vars vals)
  (cons vars vals))

(defun frame-vars (frame)
  (car frame))

(defun frame-vals (frame)
  (cdr frame))

(defun frame-append-var-val! (frame var val)
  (rplaca frame (cons var (frame-vars frame)))
  (rplacd frame (cons val (frame-vals frame)))
  'ok)

(defun frame-define! (frame var val)
  (frame-set! frame var val t))

(defun frame-set! (frame var val &optional (define nil))
  (labels ((iter (vars vals)
	     (if (null vars)
		 (if define
		     (frame-add-var-val! frame var val)
		     (error "undefined variable: ~a" var))
		 (if (eq var (car vars))
		     (rplaca vals val)
		     (iter (cdr vars) (cdr vals))))))
    (iter (frame-vars frame) (frame-vals frame))
    'ok))

(defun frame-get (frame var)
  (labels ((iter (vars vals)
	     (if (null vars)
		 (values nil nil)
		 (if (eq (car vars) var)
		     (values (car vals) t)
		     (iter (cdr vars) (cdr vals))))))
    (iter (frame-vars frame) (frame-vals frame))))

(defun t-eval (exp env)
  (cond
    ((self-evaluating-p exp) exp)
    ((variable-p exp) (env-get env exp))
    ((quoted-p exp) (object-of-quoted exp))
    ((definition-p exp) (eval-definition exp env))
    (t 'not-implemented)))

(defun self-evaluating-p (exp)
  (or (numberp exp)
      (stringp exp)))

(defun variable-p (exp)
  (symbolp exp))

(defun tagged-list-p (exp tag)
  (eq (car exp) tag))

(defun quoted-p (exp)
  (tagged-list-p exp 'quote))

(defun object-of-quoted (exp)
  (cadr exp))

(defun definition-p (exp)
  (tagged-list-p exp 'define))

(defun eval-definition (exp env)
  (let ((var (cadr exp))
	(val (eval (caddr exp))))
    (frame-define! (env-first-frame env) var val)))

(defun t-print (exp)
  (print exp))

(defun t-apply ())

(defun repl ()
  (let ((env (env-extend (make-env) nil nil)))
   (loop
;;     (fresh-line)
     (format t "LISP> ")
     (t-print (t-eval (read) env))
     (fresh-line))))

