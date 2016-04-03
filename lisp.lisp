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
    ((lambda-p exp) (eval-lambda exp env))
    ((begin-p exp) (eval-sequence (begin-actions exp) env))
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
      (make-lambda (cdadr exp)
		   (caddr exp))))

(defun make-lambda (parameters body)
  `(lambda (,parameters . ,body)))

(defun lambda-parameters (exp)
  (caadr exp))

(defun lambda-body (exp)
  (cdadr exp))

(defun assignment-p (exp)
  (tagged-list-p exp 'set!))

(defun assignment-var (exp)
  (cadr exp))

(defun assignment-val (exp)
  (caddr exp))

(defun eval-definition (exp env)
  (let ((var (definition-var exp))
	(val (definition-val exp)))
    (frame-define! (env-first-frame env)
		   var
		   (t-eval val env))))

(defun eval-assignment (exp env)
  (let ((var (assignment-var exp))
	(val (eval (assignment-val exp))))
    (frame-set! (env-first-frame env) var val)))

(defun lambda-p (exp)
  (tagged-list-p exp 'lambda))

(defun eval-lambda (exp env)
  (make-procedure exp env))

(defun make-procedure (exp env)
  (let ((parameters (lambda-parameters exp))
	(body (lambda-body exp)))
    ;; (format t "lambda: ~a~%" exp)
    ;; (format t "lambda parameters: ~a~%" parameters)
    ;; (format t "lambda body: ~a~%" body)
   `(procedure ,parameters ,body ,env)))

(defun procedure-parameters (procedure)
  (cadr procedure))

(defun procedure-body (procedure)
  (caddr procedure))

(defun procedure-env (procedure)
  (cadddr procedure))

(defun begin-p (exp)
  (tagged-list-p exp 'begin))

(defun begin-actions (exp)
  (cdr exp))

(defun last-exp-p (exps)
  (null (cdr exps)))

(defun first-exp (exps)
  (car exps))

(defun rest-exps (exps)
  (cdr exps))

(defun eval-sequence (exps env)
  (if (last-exp-p exps)
     (t-eval (first-exp exps) env)
     (progn
       (t-eval (first-exp exps) env)
       (eval-sequence (rest-exps exps) env))))

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

(defun compound-procedure-p (exp)
  (tagged-list-p exp 'procedure))

(defun t-print (exp)
  (prin1 exp))

(defun t-apply (procedure args)
  (cond
    ((primitive-procedure-p procedure)
     (apply-primitive-procedure procedure args))
    ((compound-procedure-p procedure)
     (apply-compound-procedure procedure args))
    (t (error "unknown procedure type: ~a" procedure))))

(defun apply-primitive-procedure (procedure args)
  (apply (primitive-implementation procedure) args))

(defun apply-compound-procedure (procedure args)
  (let ((vars (procedure-parameters procedure))
	(body (procedure-body procedure))
	(env (procedure-env procedure)))
    (let ((extended-env (env-extend env vars args)))
      (format t "vars: ~a~%" vars)
      (format t "args: ~a~%" args)
      (format t "body: ~a~%" body)
      (format t "env: ~a~%" extended-env)
      (eval-sequence body extended-env))))

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
  (let ((*print-circle* t)
	(env (make-global-env *primitive-procedures*)))
    (loop
      (fresh-line)
      (format t "LISP> ")
      (t-print (t-eval (read) env)))))
