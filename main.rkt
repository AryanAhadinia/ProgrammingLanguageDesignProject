#lang racket

; adding required dependencies
(require (lib "eopl.ss" "eopl"))
(require parser-tools/lex (prefix-in : parser-tools/lex-sre) parser-tools/yacc)
(require racket/trace)

; store-value
(define-datatype store-value store-value?
  [none-val]
  [numeric-val
   [val number?]]
  [bool-val
   [val boolean?]]
  [list-val
   [val (list-of store-value?)]]
  [function-val
   [function-name symbol?]
   [bound-vars (list-of symbol?)]
   [default-vals (list-of store-value?)]
   [body statements?]
   [saved-env environment?]])

; unwrap
(define store-value->function-val->function-name
  (lambda (val)
    (cases store-value val
      (function-val (function-name bound-vars default-vals body saved-env) function-name)
      (else 'error))))

(define store-value->bool
  (lambda (val)
    (cases store-value val
      (bool-val (the-val) the-val)
      (else 'error))))

(define store-value->number
  (lambda (val)
    (cases store-value val
      (numeric-val (the-val) the-val)
      (else 'error))))

(define store-value->list
  (lambda (val)
    (cases store-value val
      (list-val (the-val) the-val)
      (else 'error))))

; store
(define the-store 'uninitialized)

(define empty-store
  (lambda ()
    '()))

(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

(define newref (lambda (val)
    (let ([next-ref (length the-store)])
      (set! the-store (append the-store (list val)))
      next-ref)))

(define deref
  (lambda (ref)
    (list-ref the-store ref)))

(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref "illegal reference ~s in store ~s" ref the-store)))

(define setref!
  (lambda (ref val)
    (set! the-store
          (letrec ([setref-inner (lambda (store1 ref1)
                                   (cond [(null? store1) (report-invalid-reference ref the-store)]
                                         [(zero? ref1) (cons val (cdr store1))]
                                         [else (cons (car store1) (setref-inner (cdr store1) (- ref1 1)))]))])
            (setref-inner the-store ref)))))

(define (reference? val) (cond
                           ((not (integer? val)) #f)
                           ((< val 0) #f)
                           (else #t)))

; environment
(define-datatype environment environment?
  [empty-env]
  [extend-env
   [var symbol?]
   [val reference?]
   [rest-env environment?]]
  [interrupt-env
   [env environment?]]
  [interrupt-with-value
   [val store-value?]
   [env environment?]])

(define apply-env-ignore-interrupt
  (lambda (env search-var)
    (cases environment env
      (empty-env () (eopl:error 'apply-env "No binding for ~s" search-var))
      (extend-env (var val rest-env) (if (eqv? search-var var)
                                         val
                                         (apply-env (rest-env search-var))))
      (interrupt-env (the-env) (apply-env (the-env search-var)))
      (interrupt-with-value (val the-env) (apply-env (the-env search-var))))))

(define apply-env
  (lambda (env search-var)
    (cases environment env
      (extend-env (var val rest-env) (if (eqv? search-var var)
                                         val
                                         (apply-env rest-env search-var)))
      (else (eopl:error 'apply-env "No binding for ~s" search-var)))))

(define bounded?
  (lambda (env search-var)
    (cases environment env
      (extend-env (var val rest-env) (if (eqv? search-var var)
                                         #t
                                         (bounded? rest-env search-var)))
      (else #f))))

(define check-interrupt-free
  (lambda (env)
    (cases environment env
      (interrupt-env (the-env) #f)
      (interrupt-with-value (val the-env) #f)
      (else #t))))

(define interrupt-with-value?
  (lambda (env)
    (cases environment env
      (interrupt-with-value (val the-env) #t)
      (else #f))))

(define interrupt->value
  (lambda (env)
    (cases environment env
      (interrupt-with-value (val the-env) val)
      (else 'error))))

(define interrupt->numeric-val->value
  (lambda (env)
    (let ((store-val (interrupt->value env)))
      (cases store-value store-val
        (numeric-val (val) val)
        (else 'error)))))

(define remove-interrupt
  (lambda (env)
    (cases environment env
      (interrupt-with-value (val the-env) the-env)
      (interrupt-env (the-env) the-env)
      (else env))))

(define concat-envs
  (lambda (prior-env posterior-env)
    (cases environment prior-env
      (empty-env () posterior-env)
      (extend-env (var val rest-env) (extend-env var val (concat-envs rest-env posterior-env)))
      (interrupt-env (env) (interrupt-env (concat-envs (env posterior-env))))
      (interrupt-with-value (val env) (interrupt-with-value val (concat-envs (env posterior-env)))))))
   
; datatypes
(define-datatype program program?
  [prog
   [statements statements?]])

(define-datatype statements statements?
  [single-stmt
   [stmt statement?]]
  [multi-stmts
   [stmts statements?]
   [stmt statement?]])

(define-datatype statement statement?
  [simple-stmt
   [stmt simple-statement?]]
  [compound-stmt
   [stmt compound-statement?]])

(define-datatype simple-statement simple-statement?
  [assign-stmt
   [assign-var symbol?]
   [assign-val expression?]]
  [return-stmt
   [statement return-statement?]]
  [global-stmt
   [variable symbol?]]
  [pass-stmt]
  [break-stmt]
  [continue-stmt])

(define-datatype return-statement return-statement?
  [return-with-value-stmt
   [return-value expression?]]
  [return-without-value-stmt])

(define-datatype compound-statement compound-statement?
  [function-def-with-param-stmt
   [id symbol?]
   [params params?] 
   [statements statements?]]
  [function-def-without-param-stmt
   [id symbol?]
   [statements statements?]]
  [if-stmt
   [condition expression?]
   [on-true statements?]
   [on-false statements?]]
  [for-stmt
   [iterator symbol?]
   [iterating expression?]
   [body statements?]])

(define-datatype param-with-default param-with-default?
  [param
   [id symbol?]
   [exp expression?]])

(define param-with-default->id
  (lambda (prm)
    (cases param-with-default prm
      (param (id exp) id)
      (else 'error))))

(define param-with-default->exp
  (lambda (prm)
    (cases param-with-default prm
      (param (id exp) exp)
      (else 'error))))

(define-datatype params params?
  [single-param
   [param-with-defualt param-with-default?]]
  [multi-params
   [rest-params params?]
   [last-param param-with-default?]])

(define params->ids
  (lambda (prms)
      (cases params prms
        (single-param (prm) (list (param-with-default->id prm)))
        (multi-params (the-prms prm) (append (params->ids the-prms) (list (param-with-default->id prm)))))))

(define params->exps
  (lambda (prms)
      (cases params prms
        (single-param (prm) (list (param-with-default->exp prm)))
        (multi-params (the-prms prm) (append (params->exps the-prms) (list (param-with-default->exp prm)))))))

(define params->default-vals
  (lambda (exps env)
    (map (lambda (exp) (value-of-expression exp env)) exps)))

(define-datatype expression expression?
  [disjunction-exp
   [disjuntion-op disjunction-expression?]])

(define-datatype disjunction-expression disjunction-expression?
  [simple-disjunction-exp
   [conjuntion-op conjunction-expression?]]
  [complex-disjunction-exp
   [disjunction-op disjunction-expression?]
   [conjuntion-op conjunction-expression?]])

(define-datatype conjunction-expression conjunction-expression?
  [simple-conjunction-exp
   [inversion-op inversion-expression?]]
  [complex-conjunction-exp
   [conjuntion-op conjunction-expression?]
   [inversion-op inversion-expression?]])

(define-datatype inversion-expression inversion-expression?
  [not-inversion-exp
   [inversion-op inversion-expression?]]
  [comprasion-inversion-exp
   [comparison-op comprasion-expression?]])

(define-datatype comprasion-expression comprasion-expression?
  [comparing-comparison-exp
   [sum-op sum-expression?]
   [pairs-op compare-operator-sum-pairs?]]
  [no-comparison-exp
   [sum-op sum-expression?]])

(define-datatype compare-operator-sum-pairs compare-operator-sum-pairs?
  [single-operator-sum-pair
   [pair compare-operator-sum-pair?]]
  [multi-operator-sum-pairs
   [rest-pairs compare-operator-sum-pairs?]
   [last-pair compare-operator-sum-pair?]])

(define-datatype compare-operator-sum-pair compare-operator-sum-pair?
  [eq-sum
   [sum-op sum-expression?]]
  [lt-sum
   [sum-op sum-expression?]]
  [gt-sum
   [sum-op sum-expression?]])

(define-datatype sum-expression sum-expression?
  [sum+
   [sum-op sum-expression?]
   [term-op term-expression?]]
  [sum-
   [sum-op sum-expression?]
   [term-op term-expression?]]
  [sum-nop
   [term-op term-expression?]])

(define-datatype term-expression term-expression?
  [term*
   [term-op term-expression?]
   [factor-op factor-expression?]]
  [term/
   [term-op term-expression?]
   [factor-op factor-expression?]]
  [term-nop
   [factor-op factor-expression?]])

(define-datatype factor-expression factor-expression?
  [factor+
   [factor-op factor-expression?]]
  [factor-
   [factor-op factor-expression?]]
  [factor-nop
   [power-op power-expression?]])

(define-datatype power-expression power-expression?
  [power**
   [atom-op atom?]
   [factor-op factor-expression?]]
  [power-nop
   [primary-op primary?]])

(define-datatype primary primary?
  [atomic-primary
   [atom-op atom?]]
  [list-call
   [list-op primary?]
   [index-op expression?]]
  [function-with-arg-call
   [function-op primary?]
   [arguments-op arguments?]]
  [function-without-arg-call
   [function-op primary?]])

(define-datatype arguments arguments?
  [single-arg
   [arg expression?]]
  [multi-args
   [rest-args arguments?]
   [last-arg expression?]])

(define-datatype atom atom?
  [atomic-id
   [id symbol?]]
  [atomic-true]
  [atomic-false]
  [atomic-none]
  [atomic-number
   [num number?]]
  [atomic-list
   [list-type list-type?]])

(define-datatype list-type list-type?
  [dataful-list
   [exps expressions?]]
  [dataless-list])

(define-datatype expressions expressions?
  [single-exp
   [exp expression?]]
  [multi-exps
   [rest-exps expressions?]
   [last-exp expression?]])

; execute
(define execute-program
  (lambda (pgm)
    (begin
      (initialize-store!)
      (cases program pgm
        (prog (stmts) (execute-statements stmts (empty-env)))))))

(define execute-statements
  (lambda (stmts env)
    (cases statements stmts
      (single-stmt (stmt) (execute-statement stmt env))
      (multi-stmts (stmts stmt) (execute-statement stmt (execute-statements stmts env))))))

(define execute-statement
  (lambda (stmt env)
    (if (check-interrupt-free env)
        (cases statement stmt
          (simple-stmt (stmt) (execute-simple-statement stmt env))
          (compound-stmt (stmt) (execute-compound-statement stmt env)))
        env)))

(define execute-simple-statement
  (lambda (stmt env)
    (cases simple-statement stmt
      (assign-stmt (var val) (if (bounded? env var)
                                 (begin (setref! (apply-env env var) (value-of-expression val env))
                                        env)
                                 (extend-env var (newref (value-of-expression val env)) env)))
      (return-stmt (return-stmt) (execute-return-statement return-stmt env))
      (global-stmt (var) (extend-env var (apply-env-ignore-interrupt var) env))
      (pass-stmt () env)
      (break-stmt () (interrupt-with-value (numeric-val 0) env))
      (continue-stmt () (interrupt-with-value (numeric-val 1) env)))))
  
(define execute-compound-statement
  (lambda (stmt env)
    (cases compound-statement stmt
      (function-def-with-param-stmt (id params stmts)
                                    (extend-env id (newref (function-val id (params->ids params) (params->default-vals (params->exps params) env) stmts env)) env))
      (function-def-without-param-stmt (id stmts)
                                       (extend-env id (newref (function-val id '() '() stmts env)) env))
      (if-stmt (condition on-true on-false)
               (remove-interrupt (execute-statements (if (store-value->bool (value-of-expression condition env)) on-true  on-false) env)))
      (for-stmt (iterator iterating body)
                (let ([iterating-list (store-value->list (value-of-expression iterating env))])
                  (if (null? iterating-list)
                      env
                      (let ([new-env (execute-statements body (extend-env iterator (newref (car iterating-list)) env))])
                        (if (check-interrupt-free new-env)
                            (execute-compound-statement (for-stmt iterator (list-val (cdr iterating-list))) new-env)
                            (if (= 0 (interrupt->numeric-val->value new-env))
                                new-env
                                (execute-compound-statement (for-stmt iterator (list-val (cdr iterating-list))) (remove-interrupt new-env)))))))))))

(define execute-return-statement
  (lambda (stmt env)
    (cases return-statement stmt
      (return-with-value-stmt (val-exp) (interrupt-with-value (value-of-expression val-exp env) env))
      (return-without-value-stmt () (interrupt-env env)))))

(define value-of-expression
  (lambda (exp env)
    (cases expression exp
      (disjunction-exp (disjunction-op) (value-of-disjunction-expression disjunction-op env)))))

(define value-of-disjunction-expression
  (lambda (disjunction-exp env)
    (cases disjunction-expression disjunction-exp
      (simple-disjunction-exp (conjunction-op)
                              (value-of-conjunction-expression conjunction-op env))
      (complex-disjunction-exp (disjunction-op conjunction-op)
                               (bool-val (or
                                          (store-value->bool (value-of-disjunction-expression disjunction-op env))
                                          (store-value->bool (value-of-conjunction-expression conjunction-op env))))))))

(define value-of-conjunction-expression
  (lambda (conjunction-exp env)
    (cases conjunction-expression conjunction-exp
      (simple-conjunction-exp (inversion-op)
                              (value-of-inversion-expression inversion-op env))
      (complex-conjunction-exp (conjunction-op inversion-op)
                               (bool-val (and
                                          (store-value->bool (value-of-conjunction-expression conjunction-op env))
                                          (store-value->bool (value-of-inversion-expression inversion-op env))))))))

(define value-of-inversion-expression
  (lambda (inversion-exp env)
    (cases inversion-expression inversion-exp
      (not-inversion-exp (inversion-op) (bool-val (not (store-value->bool (value-of-inversion-expression inversion-op env)))))
      (comprasion-inversion-exp (comparison-op) (value-of-comprasion-expression comparison-op env)))))

(define value-of-comprasion-expression
  (lambda (comprasion-exp env)
    (cases comprasion-expression comprasion-exp
      (comparing-comparison-exp (sum-op pairs-op)
                                (cases compare-operator-sum-pairs pairs-op
                                  (single-operator-sum-pair (pair) (value-of-compare-operator-sum-pair (value-of-sum-expression sum-op env) pair env))
                                  (multi-operator-sum-pairs (rest-pairs last-pair)
                                                            (if (store-value->bool (value-of-comprasion-expression (comparing-comparison-exp sum-op rest-pairs) env))
                                                                (value-of-compare-operator-sum-pair (last-sum-of-pairs rest-pairs) last-pair env)
                                                                (bool-val #f)))))
      (no-comparison-exp (sum-op) (value-of-sum-expression sum-op env)))))

(define value-of-compare-operator-sum-pair
  (lambda (external-sum pair env)
    (cases compare-operator-sum-pair pair
      (eq-sum (sum-op) (bool-val (= (store-value->number external-sum)
                                    (store-value->number (value-of-sum-expression sum-op env)))))
      (lt-sum (sum-op) (bool-val (< (store-value->number external-sum)
                                    (store-value->number (value-of-sum-expression sum-op env)))))
      (gt-sum (sum-op) (bool-val (> (store-value->number external-sum)
                                    (store-value->number (value-of-sum-expression sum-op env))))))))

(define last-sum-of-pairs
  (lambda (pairs env)
    (value-of-sum-expression (cases compare-operator-sum-pair (cases compare-operator-sum-pairs pairs
                                       (single-operator-sum-pair (pair) pair)
                                       (multi-operator-sum-pairs (rest-pairs last-pair) last-pair))
                               (eq-sum (sum-op) sum-op)
                               (lt-sum (sum-op) sum-op)
                               (gt-sum (sum-op) sum-op))
                             env)))

(define value-of-sum-expression
  (lambda (sum-exp env)
    (cases sum-expression sum-exp
      (sum+ (sum-op term-op) (let ([op1 (value-of-sum-expression sum-op env)]
                                   [op2 (value-of-term-expression term-op env)])
                               (cases store-value op1
                                 (numeric-val (val) (numeric-val (+ (store-value->number op1)
                                                                    (store-value->number op2))))
                                 (bool-val (val) (bool-val (or (store-value->bool op1)
                                                               (store-value->bool op2))))
                                 (list-val (val) (list-val (append (store-value->list op1)
                                                                   (store-value->list op2))))
                                 (else 'errorsum))))
      (sum- (sum-op term-op) (numeric-val (- (store-value->number (value-of-sum-expression sum-op env))
                                             (store-value->number (value-of-term-expression term-op env)))))
      (sum-nop (term-op) (value-of-term-expression term-op env)))))

(define value-of-term-expression
  (lambda (term-exp env)
    (cases term-expression term-exp
      (term* (term-op factor-op) (let ([op1 (value-of-term-expression term-op env)]
                                       [op2 (value-of-factor-expression factor-op env)])
                                   (cases store-value op1
                                     (numeric-val (val) (numeric-val (* (store-value->number op1)
                                                                        (store-value->number op2))))
                                     (bool-val (val) (bool-val (and (store-value->bool op1)
                                                                    (store-value->bool op2))))
                                     (else 'errorterm))))
      (term/ (term-op factor-op) (numeric-val (/ (store-value->number (value-of-term-expression term-op env))
                                                 (store-value->number (value-of-factor-expression factor-op env)))))
      (term-nop (factor-op) (value-of-factor-expression factor-op env)))))

(define value-of-factor-expression
  (lambda (factor-exp env)
    (cases factor-expression factor-exp
      (factor+ (factor-op) (numeric-val (* +1 (store-value->number (value-of-factor-expression factor-op env)))))
      (factor- (factor-op) (numeric-val (* -1 (store-value->number (value-of-factor-expression factor-op env)))))
      (factor-nop (power-op) (value-of-power-expression power-op env)))))

(define value-of-power-expression
  (lambda (power-exp env)
    (cases power-expression power-exp
      (power** (atom-op factor-op) (numeric-val (expt (store-value->number (value-of-atom atom-op env))
                                                      (store-value->number (value-of-factor-expression factor-op env)))))
      (power-nop (primary-op) (value-of-primary primary-op env)))))

(define value-of-primary
  (lambda (prmy-exp env)
    (cases primary prmy-exp
      (atomic-primary (atom-op) (value-of-atom atom-op env))
      (list-call (list-op index-op) (list-ref (store-value->list (value-of-primary list-op env))
                                              (store-value->number (value-of-expression index-op env))))
      (function-with-arg-call (function-op arguments-op) (let ([func-val (value-of-primary function-op env)])
                                                           (cases store-value func-val
                                                             (function-val (function-name bound-vars default-vals body saved-env)
                                                                           (let ([new-env (execute-statements
                                                                                           body
                                                                                           (extend-env-for-call
                                                                                            func-val
                                                                                            bound-vars
                                                                                            (value-of-arguments arguments-op env)
                                                                                            default-vals
                                                                                            saved-env
                                                                                            env))])
                                                                             (if (check-interrupt-free new-env)
                                                                                 (none-val)
                                                                                 (if (interrupt-with-value? new-env)
                                                                                     (interrupt->value new-env)
                                                                                     (none-val)))))
                                                             (else 'errorprim))))
      (function-without-arg-call (function-op) (let ([func-val (value-of-primary function-op env)])
                                                 (cases store-value func-val
                                                   (function-val (function-name bound-vars default-vals body saved-env)
                                                                 (let ([new-env (execute-statements
                                                                                 body
                                                                                 (extend-env-for-call
                                                                                  func-val
                                                                                  bound-vars
                                                                                  '()
                                                                                  default-vals
                                                                                  saved-env
                                                                                  env))])
                                                                   (if (check-interrupt-free new-env)
                                                                       (none-val)
                                                                       (if (interrupt-with-value? new-env)
                                                                           (interrupt->value new-env)
                                                                           (none-val)))))
                                                   (else 'errorprim)))))))

(define extend-env-for-call
  (lambda (func-val vars bound-vals default-vals saved-env runtime-env)
    (extend-env-with-vals
     vars
     bound-vals
     (extend-env-with-vals
      vars
      default-vals
      (extend-env
       (store-value->function-val->function-name func-val)
       (newref func-val)
       (concat-envs
        saved-env
        (interrupt-env runtime-env)))))))

(define extend-env-with-vals
  (lambda (vars vals env)
    (cond
      [(null? vals) env]
      [else (extend-env-with-vals (cdr vars) (cdr vals) (extend-env (car vars) (newref (car vals)) env))])))

(define value-of-arguments
  (lambda (args env)
    (cases arguments args
      (single-arg (arg) (list (value-of-expression arg env)))
      (multi-args (rest last) (append (value-of-arguments rest env) (list (value-of-expression last env)))))))

(define value-of-atom
  (lambda (atom-exp env)
    (cases atom atom-exp
      (atomic-id (id) (deref(apply-env env id)))
      (atomic-true () (bool-val #t))
      (atomic-false () (bool-val #f))
      (atomic-none () (none-val))
      (atomic-number (num) (numeric-val num))
      (atomic-list (list-exps) (value-of-list-type list-exps)))))

(define value-of-list-type
  (lambda (list-exps env)
    (cases list-type list-exps
      (dataful-list (exps) (list-val (value-of-expressions exps env)))
      (dataless-list () (list-val '())))))

(define value-of-expressions
  (lambda (exps env)
    (cases expressions exps
      (single-exp (exp)
                  (list (value-of-expression exp env)))
      (multi-exps (rest-exps last-exp)
                  (append (value-of-expressions rest-exps env)
                          (list (value-of-expression last-exp env)))))))

; lexer
(define main-lexer
           (lexer
            ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUM (string->number lexeme)))
            ((:& (repetition 1 +inf.0 (union (char-range #\a #\z) (char-range #\A #\Z) #\_ (char-range #\0 #\9) ))
                 (complement (:or (:? "if") (:? "else") (:? "def") (:? "break") (:? "pass") (:? "continue") (:? "return") (:? "global")
                                  (:? "for") (:? "in") (:? "or") (:? "and") (:? "not") (:? "True") (:? "False") (:? "None")))) (token-ID (string->symbol lexeme)))
            ("+" (token-+))
            ("-" (token--))
            ("*" (token-*))
            ("**" (token-**))
            ("if" (token-if))
            ("else" (token-else))
            (";" (token-sc))
            ("pass" (token-pass))
            ("break" (token-break))
            ("continue" (token-continue))
            ("=" (token-=))
            ("return" (token-return))
            ("global" (token-global))
            ("def" (token-def))
            ("(" (token-open_par))
            (")" (token-close_par))
            (":" (token-dd))
            ("():" (token-o_c_p_d))
            ("," (token-cama))
            ("for" (token-for))
            ("in" (token-in))
            ("or" (token-or))
            ("and" (token-and))
            ("not" (token-not))
            ("==" (token-==))
            ("<" (token-<))
            (">" (token->))
            ("/" (token-/))
            ("[" (token-open_q))
            ("]" (token-close_q))
            ("[]" (token-o_c_p))
            ("True" (token-True))
            ("False" (token-False))
            ("None" (token-None))
            (whitespace (main-lexer input-port))
            ((eof) (token-EOF))))

; tokens
(define-tokens value-tokens (NUM ID))
(define-empty-tokens op-tokens (EOF sc pass break continue = return global def open_par close_par dd o_c_p_d cama
                            if else for in or and not == < > + - * / ** open_q close_q o_c_p True False None))

; parser
(define main-parser
           (parser
            (start Program)
            (end EOF)
            (error void)
            (tokens op-tokens value-tokens)
           (grammar
            (Program ((Statements) (prog $1)))
            (Statements ((Statement sc) (single-stmt $1))
                        ((Statements Statement sc) (multi-stmts $1 $2)))
            (Statement ((Compound_stmt) (compound-stmt $1))
                       ((Simple_stmt) (simple-stmt $1)))
            (Simple_stmt ((ID = Expression) (assign-stmt $1 $3))
                         ((Return_stmt) (return-stmt $1))
                         ((global ID) (global-stmt $2))
                         ((pass) (pass-stmt))
                         ((break) (break-stmt))
                         ((continue) (continue-stmt)))
            (Compound_stmt ((def ID open_par Params close_par dd Statements) (function-def-with-param-stmt $2 $4 $7))
                           ((def ID o_c_p_d Statements) (function-def-without-param-stmt $2 $4))
                          ((if Expression dd Statements else dd Statements) (if-stmt $2 $4 $7))
                          ((for ID in Expression dd Statements) (for-stmt $2 $4 $6)))
            (Return_stmt ((return) (return-without-value-stmt))
                        ((return Expression) (return-with-value-stmt $2)))
            (Params ((Param_with_default) (single-param $1))
                   ((Params cama Param_with_default) (multi-params $1 $3)))
            (Param_with_default ((ID = Expression) (param $1 $3)))
            (Expression ((Disjunction) (disjunction-exp $1)))
            (Disjunction ((Conjunction) (simple-disjunction-exp $1))
                         ((Disjunction or Conjunction) (complex-disjunction-exp $1 $3)))
            (Conjunction ((Inversion) (simple-conjunction-exp $1))
                         ((Conjunction and Inversion) (complex-conjunction-exp $1 $3)))
            (Inversion ((not Inversion) (not-inversion-exp $2))
                       ((Comparison) (comprasion-inversion-exp $1)))
            (Comparison ((Sum Compare_op_Sum_Pairs) (comparing-comparison-exp $1 $2))
                        ((Sum) (no-comparison-exp $1)))
            (Compare_op_Sum_Pairs ((Compare_op_Sum_Pair) (single-operator-sum-pair $1))
                                  ((Compare_op_Sum_Pairs Compare_op_Sum_Pair) (multi-operator-sum-pairs $1 $2)))
            (Compare_op_Sum_Pair ((== Sum) (eq-sum $2))
                                 ((< Sum) (lt-sum $2))
                                 ((> Sum) (gt-sum $2)))
            (Sum ((Sum + Term) (sum+ $1 $3))
                 ((Sum - Term) (sum- $1 $3))
                 ((Term) (sum-nop $1)))
            (Term ((Term * Factor) (term* $1 $3))
                  ((Term / Factor) (term/ $1 $3))
                  ((Factor) (term-nop $1)))
            (Factor ((+ Factor) (factor+ $2))
                    ((- Factor) (factor- $2))
                    ((Power) (factor-nop $1)))
            (Power ((Atom ** Factor) (power** $1 $3))
                   ((Primary) (power-nop $1)))
            (Primary ((Atom) (atomic-primary $1))
                     ((Primary open_q Expression close_q) (list-call $1 $3))
                     ((Primary open_par close_par) (function-without-arg-call $1))
                     ((Primary open_par Arguments close_par) (function-with-arg-call $1 $3)))
            (Arguments ((Expression) (single-arg $1))
                       ((Arguments cama Expression) (multi-args $1 $3)))
            (Atom ((ID) (atomic-id $1))
                  ((True) (atomic-true))
                  ((False) (atomic-false))
                  ((None) (atomic-none))
                  ((NUM) (atomic-number $1))
                  ((List) (atomic-list $1)))
            (List ((open_q Expressions close_q) (dataful-list $2))
                  ((open_q close_q) (dataless-list)))
            (Expressions ((Expressions cama Expression) (multi-exps $1 $3))
                         ((Expression) (single-exp $1))))))

; test
(define (evaluate path)
    (define lex-this (lambda (lexer input) (lambda () (lexer input))))
    (define my-lexer (lex-this main-lexer (open-input-string (file->string path))))
  (let ((parser-res (main-parser my-lexer))) (begin
                                               (trace value-of-primary)
                                               (execute-program parser-res)
                                               ;(display (list-ref the-store 0))    
                                                    )
                                                    ))

(evaluate "testbench-recurssion.txt")
