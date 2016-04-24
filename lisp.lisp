(defun make-env ()
  '())

(defun env-frame-empty-p (env)
  (null env))

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

(defun frame-get (frame var)
  (labels ((iter (vars vals)
	     (if (null vars)
		 (values nil nil)
		 (if (eq (car vars) var)
		     (values (car vals) t)
		     (iter (cdr vars) (cdr vals))))))
    (iter (frame-vars frame) (frame-vals frame))))

(defun t-eval (exp env)
  (funcall (analyze exp) env))

(defun analyze (exp)
  (format t "analyze: ~a~%" exp)
  (cond
    ((self-evaluating-p exp) (analyze-self-evaluating exp))
    ((variable-p exp) (analyze-variable exp))
    ((quoted-p exp) (analyze-quoted exp))
    ((assignment-p exp) (analyze-assignment exp))
    ((definition-p exp) (analyze-definiton exp))
    ((lambda-p exp) (analyze-lambda exp))
    ((if-p exp) (analyze-if exp))
    ((cond-p exp) (analyze (cond->if exp)))
    ((and-p exp) (analyze-and exp))
    ((or-p exp) (analyze-or exp))
    ((let-p exp) (analyze (let->combination exp)))
    ((let*-p exp) (analyze (let*->nested-let exp)))
    ((begin-p exp) (analyze-sequence (begin-actions exp)))
    ((application-p exp) (analyze-application exp))
    (t 'not-implemented)))

(defun analyze-self-evaluating (exp)
  (lambda (env) exp))

(defun analyze-variable (exp)
  (lambda (env) (env-get env exp)))

(defun analyze-quoted (exp)
  (let ((qval (object-of-quoted exp)))
    (lambda (env) qval)))

(defun analyze-assignment (exp)
  (let ((var (assignment-var exp))
	(val-proc (analyze (assignment-val exp))))
    (lambda (env)
     (set-variable-value! env var (funcall val-proc env))
     'ok)))

(defun analyze-definiton (exp)
  (let ((var (definition-var exp))
	(val-proc (analyze (definition-val exp))))
    (lambda (env)
      (define-variable!
	  (env-first-frame env)
	  var (funcall val-proc env))
      'ok)))

(defun analyze-if (exp)
  (let ((pred-proc (analyze (if-predicate exp)))
	(consequent-proc (analyze (if-consequent exp)))
	(alternative-proc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true-p (funcall pred-proc env))
	  (funcall consequent-proc env)
	  (funcall alternative-proc env)))))

(defun self-evaluating-p (exp)
  (or (numberp exp)
      (stringp exp)))

(defun variable-p (exp)
  (symbolp exp))

(defun tagged-list-p (exp tag)
  (and (consp exp) (eq (car exp) tag)))

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
		   (cddr exp))))

(defun make-lambda (parameters body)
  (format t "make-lamba parameters: ~a~%" parameters)
  (format t "make-lamba body: ~a~%" body)
  `(lambda ,parameters ,@body))

(defun lambda-parameters (exp)
  (cadr exp))

(defun lambda-body (exp)
  (cddr exp))

(defun assignment-p (exp)
  (tagged-list-p exp 'set!))

(defun assignment-var (exp)
  (cadr exp))

(defun assignment-val (exp)
  (caddr exp))

(defun define-variable! (frame var val)
  (labels ((iter (vars vals)
	     (if (null vars)
		 (frame-append-var-val! frame var val)
		 (if (eq var (car vars))
		     (rplaca vals val)
		     (iter (cdr vars) (cdr vals))))))
    (iter (frame-vars frame) (frame-vals frame))))

(defun set-variable-value! (env var val)
  (labels ((iter-frame (env)
	     (labels ((iter (vars vals)
			(if (null vars)
			    (iter-frame (env-rest env))
			    (if (eq var (car vars))
				(rplaca vals val)
				(iter (cdr vars) (cdr vals))))))
	       (if (env-frame-empty-p env)
		 (error "undefined variable: ~a" var)
		 (let ((frame (env-first-frame env)))
		   (iter (frame-vars frame) (frame-vals frame)))))))
    (iter-frame env)))

(defun lambda-p (exp)
  (tagged-list-p exp 'lambda))

(defun analyze-lambda (exp)
  (lambda (env)
    (make-procedure exp env)))

(defun make-procedure (exp env)
  (let ((parameters (lambda-parameters exp))
	(body (lambda-body exp)) ;; for debug
	(body-proc (analyze-sequence (lambda-body exp))))
    (format t "lambda: ~a~%" exp)
    (format t "lambda parameters: ~a~%" parameters)
    (format t "lambda body: ~a~%" body)
   `(procedure ,parameters ,body-proc ,env)))

(defun procedure-parameters (procedure)
  (cadr procedure))

(defun procedure-body-proc (procedure)
  (caddr procedure))

(defun procedure-env (procedure)
  (cadddr procedure))

(defun make-if (pred consequent alternative)
  `(if ,pred ,consequent ,alternative))

(defun if-p (exp)
  (tagged-list-p exp 'if))

(defun if-predicate (exp)
  (cadr exp))

(defun if-consequent (exp)
  (caddr exp))

(defun if-alternative (exp)
  (let ((alternative (cadddr exp)))
    (if alternative
	alternative
	'false)))

(defun true-p (exp)
  (not (false-p exp)))

(defun false-p (exp)
  (eq exp 'false))

(defun cond-p (exp)
  (tagged-list-p exp 'cond))

(defun first-cond-clause (exp)
  (car exp))

(defun rest-cond-clauses (exp)
  (cdr exp))

(defun cond-clause-recipient-p (exp)
  (eq (cadr exp) '=>))

(defun cond-clause-pred (clause)
  (car clause))

(defun cond-clause-exps (clause)
  (cdr clause))

(defun cond-clause-recipient (clause)
  (caddr clause))

(defun else-clause-p (clause)
  (tagged-list-p clause 'else))

(defun cond->if (exp)
  (labels ((iter (clauses)
	     (if (null clauses)
		 'false
		 (let* ((clause (first-cond-clause clauses))
			(pred (cond-clause-pred clause)))
		   (if (cond-clause-recipient-p clause)
		       (let ((recipient (cond-clause-recipient clause)))
			 ;; TODO remove the site effect
			 (if (eq pred 'else)
			     `(,recipient ,pred)
			     `(if ,pred
				  (,recipient ,pred)
				  ,(iter (rest-cond-clauses clauses)))))
		       (let ((exps (cond-clause-exps clause)))
			 (if (eq pred 'else)
			     `(begin ,@exps)
			     `(if ,pred
				  (begin ,@exps)
				  ,(iter (rest-cond-clauses clauses))))))))))
    (iter (cdr exp))))

;; (cond (pred1 exp1 ...)
;;       (pred2 exp2 ...)
;;       (else exp3...))
;;
;; (if pred1 (begin exp1 ...)
;;     (if pred2 (begin exp2 ...)
;;         (begin exp3 ...)))

;; and

(defun and-p (exp)
  (tagged-list-p exp 'and))

(defun and-exps (exp)
  (cdr exp))

(defun analyze-and (exp)
  (when (null (and-exps exp))
    (error "no expressions"))
  (labels ((iter (procs env)
	      (if (null (cdr procs))
		  (funcall (car procs) env)
		  (let ((result (funcall (car procs) env)))
		    (if (true-p result)
			(iter (cdr procs) env)
			result)))))
    (let ((exp-procs (mapcar #'analyze (and-exps exp))))
      (lambda (env) (iter exp-procs env)))))

;; or

(defun or-p (exp)
  (tagged-list-p exp 'or))

(defun analyze-or (exp)
  (when (null (and-exps exp))
    (error "no expressions"))
  (labels ((iter (procs env)
	     (if (null (cdr procs))
		 (funcall (car procs) env)
		 (let ((result (funcall (car procs) env)))
		   (if (true-p result)
		       result
		       (iter (cdr procs) env))))))
    (let ((exp-procs (mapcar #'analyze (and-exps exp))))
      (lambda (env) (iter exp-procs env)))))

;; let

(defun let-p (exp)
  (tagged-list-p exp 'let))

(defun let-vars (exp)
  (cadr exp))

(defun let-body (exp)
  (cddr exp))

(defun let->combination (exp)
  (let ((vars (mapcar #'(lambda (var) (car var))
		      (let-vars exp)))
	(vals (mapcar #'(lambda (var) (cadr var))
		      (let-vars exp)))
	(body (let-body exp)))
    `(,(make-lambda vars body) ,@vals)))

;; let*

(defun let*-p (exp)
  (tagged-list-p exp 'let*))

(defun let*-vars (exp)
  (cadr exp))

(defun let*-body (exp)
  (cddr exp))

(defun let*-last-var-p (vars)
  (null (cdr vars)))

(defun let*-first-var (vars)
  (car vars))

(defun let*-rest-vars (vars)
  (cdr vars))

(defun let*->nested-let (exp)
  (labels ((iter (vars)
	     (let ((var (let*-first-var vars)))
	      (if (let*-last-var-p vars)
		  `(let (,var)
		     ,@(let*-body exp))
		  `(let (,var)
		     ,(iter (let*-rest-vars vars)))))))
    (iter (let*-vars exp))))

;; begin

(defun begin-p (exp)
  (tagged-list-p exp 'begin))

(defun begin-actions (exp)
  (cdr exp))

;; expressions

(defun last-exp-p (exps)
  (null (cdr exps)))

(defun first-exp (exps)
  (car exps))

(defun rest-exps (exps)
  (cdr exps))

(defun analyze-sequence (exps)
  (labels
      ((sequentially (proc1 proc2)
	 (lambda (env) (funcall proc1 env) (funcall proc2 env)))
       (iter (first-proc rest-procs)
	 (if (last-exp-p rest-procs)
	     first-proc
	     (iter (sequentially first-proc (car rest-procs))
		   (cdr rest-procs)))))
    (let ((procs (mapcar #'analyze exps)))
      (if (null procs)
	  (error "empty sequence")
	  (iter (car procs) (cdr procs))))))

(defun application-p (exp)
  (consp exp))

(defun application-operator (exp)
  (car exp))

(defun application-operand (exp)
  (cdr exp))

(defun analyze-application (exp)
  (let ((procedure-proc (analyze (application-operator exp)))
	(arg-procs (list-of-value-procs (application-operand exp))))
    (lambda (env)
      (t-apply
       (funcall procedure-proc env)
       (mapcar #'(lambda (proc) (funcall proc env)) arg-procs)))))

(defun list-of-value-procs (args)
  (mapcar #'analyze args))

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
  (format t "apply-pp procedure: ~a~%" procedure)
  (format t "apply-pp args: ~a~%" args)
  (apply (primitive-implementation procedure) args))

(defun apply-compound-procedure (procedure args)
  (let ((vars (procedure-parameters procedure))
	(body-proc (procedure-body-proc procedure))
	(env (procedure-env procedure)))
    (let ((extended-env (env-extend env vars args)))
      (format t "apply-cp vars: ~a~%" vars)
      (format t "apply-cp args: ~a~%" args)
      (format t "apply-cp body: ~a~%" body-proc)
      (format t "apply-cp env: ~a~%" extended-env)
      (funcall body-proc extended-env))))

(defun to-boolean (exp)
  (if (null exp)
      'false
      'true))

(defun boolean-operator (operator)
  #'(lambda (&rest args)
      (to-boolean (apply operator args))))

(defparameter *primitive-procedures*
	      `((+ ,#'+)
		(- ,#'-)
		(* ,#'*)
		(/ ,#'/)
		(= ,(boolean-operator #'=))
		(< ,(boolean-operator #'<))
		(> ,(boolean-operator #'>))
		(<= ,(boolean-operator #'<=))
		(>= ,(boolean-operator #'>=))))

(defun procedure-name (procedure)
  (car procedure))

(defun procedure-object (procedure)
  (cadr procedure))

(defun make-global-env (primitive-procedures)
  (let ((env (env-extend (make-env)
			 (mapcar #'procedure-name primitive-procedures)
			 (mapcar #'(lambda (procedure)
				     `(primitive ,(procedure-object procedure)))
				 primitive-procedures))))
    (let ((frame (env-first-frame env)))
      (define-variable! frame 'true 'true)
      (define-variable! frame 'false 'false))
    env))

(defun repl ()
  (let ((*print-circle* t)
	(env (make-global-env *primitive-procedures*)))
    (loop
      (fresh-line)
      (format t "LISP> ")
      (t-print (t-eval (read) env))
      ;; (handler-case
      ;; 	  (t-print (t-eval (read) env))
      ;; 	(error (e) (princ e))))))
      )))

;; for debug
(defun p (value)
  (format t "debug: ~a~%" value)
  value)
