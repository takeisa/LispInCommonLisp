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
		     (frame-append-var-val! frame var val)
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
    ((assignment-p exp) (eval-assignment exp env))
    ((definition-p exp) (eval-definition exp env))
    ((application-p exp) (eval-application exp env))
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

(defun definition-var (exp)
  (if (symbolp (cadr exp))
      (cadr exp)
      (caadr exp)))

(defun definition-val (exp)
  (if (symbolp (cadr exp))
      (caddr exp)
      (error "not implemented lambda")))

(defun assignment-p (exp)
  (tagged-list-p exp 'set!))

(defun assignment-var (exp)
  (cadr exp))

(defun assignment-val (exp)
  (caddr exp))

(defun eval-definition (exp env)
  (let ((var (definition-var exp))
	(val (definition-val exp)))
    (frame-define! (env-first-frame env) var val)))

(defun eval-assignment (exp env)
  (let ((var (assignment-var exp))
	(val (eval (assignment-val exp))))
    (frame-set! (env-first-frame env) var val)))

(defun application-p (exp)
  (consp exp))

(defun application-operator (exp)
  (car exp))

(defun application-operand (exp)
  (cdr exp))

(defun eval-application (exp env)
  (let ((procedure (t-eval (application-operator exp) env))
	(args (list-of-values (application-operand exp) env)))
    (t-apply procedure args)))

(defun list-of-values (args env)
  (mapcar #'(lambda (arg) (t-eval arg env)) args))

(defun primitive-procedure-p (exp)
  (tagged-list-p exp 'primitive))

(defun primitive-implementation (procedure)
  (cadr procedure))

(defun t-print (exp)
  (prin1 exp))

(defun t-apply (procedure args)
  (cond
    ((primitive-procedure-p procedure)
     (apply-primitive-procedure procedure args))
    (t (format t "t-apply: ~a ~a~%" procedure args)
       (error "not implemented"))))

(defun apply-primitive-procedure (procedure args)
  (apply (primitive-implementation procedure) args))

(defparameter *primitive-procedures*
	      `((+ ,#'+)
		(- ,#'-)
		(* ,#'*)
		(/ ,#'/)))

(defun procedure-name (procedure)
  (car procedure))

(defun procedure-object (procedure)
  (cadr procedure))

(defun make-global-env (primitive-procedures)
  (env-extend (make-env)
	      (mapcar #'procedure-name primitive-procedures)
	      (mapcar #'(lambda (procedure)
			  `(primitive ,(procedure-object procedure)))
		      primitive-procedures)))

(defun repl ()
  (let ((env (make-global-env *primitive-procedures*)))
    (loop
      (fresh-line)
      (format t "LISP> ")
      (t-print (t-eval (read) env)))))
