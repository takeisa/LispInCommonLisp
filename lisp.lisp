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
  (format t "t-eval: ~a~%" exp)
  (cond
    ((self-evaluating-p exp) exp)
    ((variable-p exp) (env-get env exp))
    ((quoted-p exp) (object-of-quoted exp))
    ((assignment-p exp) (eval-assignment exp env))
    ((definition-p exp) (eval-definition exp env))
    ((lambda-p exp) (eval-lambda exp env))
    ((if-p exp) (eval-if exp env))
    ((cond-p exp) (t-eval (cond->if exp) env))
    ((and-p exp) (eval-and exp env))
    ((or-p exp) (eval-or exp env))
    ((let-p exp) (t-eval (let->combination exp) env))
    ((let*-p exp) (t-eval (let*->nested-let exp) env))
    ((begin-p exp) (eval-sequence (begin-actions exp) env))
    ((application-p exp) (eval-application exp env))
    (t 'not-implemented)))

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

(defun eval-definition (exp env)
  (let ((var (definition-var exp))
	(val (definition-val exp)))
    (format t "eval-definition: var=~a val=~a~%" var val)
    (define-variable! (env-first-frame env)
	var
      (t-eval val env))
    (format t "eval-definition: env(after)=~a~%" env)
    'ok))

(defun eval-assignment (exp env)
  (format t "eval-assignment: exp=~a~%" exp)
  (format t "eval-assignment: env=~a~%" env)
  (let ((var (assignment-var exp))
	(val (t-eval (assignment-val exp) env)))
    (format t "eval-assignment: var=~a~%" var)
    (format t "eval-assignment: val=~a~%" val)
    (set-variable-value! env var val)
    (format t "eval-assignment: env(after)=~a~%" env)
    ))

(defun define-variable! (frame var val)
  (labels ((iter (vars vals)
	     (if (null vars)
		 (frame-append-var-val! frame var val)
		 (if (eq var (car vars))
		     (rplaca vals val)
		     (iter (cdr vars) (cdr vals))))))
    (iter (frame-vars frame) (frame-vals frame))))

(defun set-variable-value! (env var val)
  (format t "set-variable-value!: env=~a~%" env)
  (format t "set-variable-value!: var=~a val=~a~%" var val)
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

(defun eval-lambda (exp env)
  (make-procedure exp env))

(defun make-procedure (exp env)
  (let ((parameters (lambda-parameters exp))
	(body (lambda-body exp)))
    (format t "lambda: ~a~%" exp)
    (format t "lambda parameters: ~a~%" parameters)
    (format t "lambda body: ~a~%" body)
   `(procedure ,parameters ,body ,env)))

(defun procedure-parameters (procedure)
  (cadr procedure))

(defun procedure-body (procedure)
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

(defun eval-if (exp env)
  (if (true-p (force-it (t-eval (if-predicate exp) env)))
      (t-eval (if-consequent exp) env)
      (t-eval (if-alternative exp) env)))

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

(defun eval-and (exp env)
  (when (null (and-exps exp))
    (error "no expressions"))
  (labels ((iter (exps)
	     (if (last-exp-p exps)
		 (t-eval (first-exp exps) env)
		 (let ((result (t-eval (first-exp exps) env)))
		   (if (true-p result)
		       (iter (rest-exps exps))
		       result)))))
      (iter (and-exps exp))))

;; or

(defun or-p (exp)
  (tagged-list-p exp 'or))

(defun eval-or (exp env)
  (when (null (and-exps exp))
    (error "no expressions"))
  (labels ((iter (exps)
	     (if (last-exp-p exps)
		 (t-eval (first-exp exps) env)
		 (let ((result (t-eval (first-exp exps) env)))
		   (if (true-p result)
		       result
		       (iter (rest-exps exps)))))))
    (iter (and-exps exp))))

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

(defun application-operand (exp)`<
  (cdr exp))

;; (defun eval-application (exp env)
;;   (let ((procedure (t-eval (application-operator exp) env))
;; 	(args (list-of-values (application-operand exp) env)))
;;     (t-apply procedure args)))

(defun eval-application (exp env)
  (let ((procedure (actual-value (application-operator exp) env))
	(args (application-operand exp)))
    (t-apply procedure args env)))

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

;; (defun t-apply (procedure args)
;;   (cond
;;     ((primitive-procedure-p procedure)
;;      (apply-primitive-procedure procedure args))
;;     ((compound-procedure-p procedure)
;;      (apply-compound-procedure procedure args))
;;     (t (error "unknown procedure type: ~a" procedure))))

(defun t-apply (procedure args env)
  (cond
    ((primitive-procedure-p procedure)
     (apply-primitive-procedure procedure args env))
    ((compound-procedure-p procedure)
     (apply-compound-procedure procedure args env))
    (t (error "unknown procedure type: ~a" procedure))))

;; (defun apply-primitive-procedure (procedure args)
;;   (format t "apply-pp procedure: ~a~%" procedure)
;;   (format t "apply-pp args: ~a~%" args)
;;   (apply (primitive-implementation procedure) args))

(defun apply-primitive-procedure (procedure args env)
  (format t "apply-pp procedure: ~a~%" procedure)
  (format t "apply-pp args: ~a~%" args)
  (let ((actual-args (list-of-actual-args args env)))
    (format t "apply-pp actual-args: ~a~%" actual-args)
    (apply (primitive-implementation procedure) actual-args)))

(defun list-of-actual-args (args env)
  (mapcar #'(lambda (arg)
	      (actual-value arg env))
	  args))

;; (defun apply-compound-procedure (procedure args)
;;   (let ((vars (procedure-parameters procedure))
;; 	(body (procedure-body procedure))
;; 	(env (procedure-env procedure)))
;;     (let ((extended-env (env-extend env vars args)))
;;       (format t "apply-cp vars: ~a~%" vars)
;;       (format t "apply-cp args: ~a~%" args)
;;       (format t "apply-cp body: ~a~%" body)
;;       (format t "apply-cp env: ~a~%" extended-env)
;;       (eval-sequence body extended-env))))

(defun apply-compound-procedure (procedure args env)
  (let ((vars (procedure-parameters procedure))
	(body (procedure-body procedure))
	(env (procedure-env procedure)))
    (let ((extended-env (env-extend env vars (list-of-delayed-args args env))))
      (format t "apply-cp vars: ~a~%" vars)
      (format t "apply-cp args: ~a~%" args)
      (format t "apply-cp body: ~a~%" body)
      (format t "apply-cp env: ~a~%" extended-env)
      (eval-sequence body extended-env))))

(defun list-of-delayed-args (args env)
  (mapcar #'(lambda (arg)
	      (delay-it arg env))
	  args))

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

;; lazy evaluation

(defun actual-value (exp env)
;;  (format t "actual-value: env=~a~%" env)
;;  (format t "actual-value: exp=~a~%" exp)
  (let ((evaluated-value (t-eval exp env)))
;;    (format t "actual-value: evaluated-value=~a~%" evaluated-value)
    (force-it evaluated-value)))

(defun delay-it (exp env)
  (list 'thunk exp env))

(defun thunk-p (obj)
  (tagged-list-p obj 'thunk))

(defun thunk-exp (thunk)
  (cadr thunk))

(defun thunk-env (thunk)
  (caddr thunk))

(defun evaluated-thunk-p (thunk)
  (tagged-list-p thunk 'evaluated-thunk))

(defun thunk-value (evaluated-thunk)
  (cadr evaluated-thunk))

(defun force-it (obj)
  (cond ((thunk-p obj)
	 (let ((result (actual-value
			(thunk-exp obj)
			(thunk-env obj))))
	   (setf (car obj) 'evaluated-thunk)
	   (setf (cadr obj) result)
	   (setf (caddr obj) nil)
	   result))
	((evaluated-thunk-p obj)
	 (thunk-value obj))
	(t obj)))

;; repl

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
      (t-print (force-it (t-eval (read) env)))
      ;; (handler-case
      ;; 	  (t-print (t-eval (read) env))
      ;; 	(error (e) (princ e))))))
      )))

;; for debug
(defun p (value)
  (format t "debug: ~a~%" value)
  value)

;; (define (unless condition usual exceptional)
;;     (if condition exceptional usual))

;; (unless (= 1 0) 'hoge (/ 1 0))

;; (define (integer n)
;;     (cons n (integer (+ n 1))))


;; (define count 0) (define (test1) (set! count 1) count) (test1)
