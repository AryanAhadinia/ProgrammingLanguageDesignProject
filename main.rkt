#lang racket

; Adding required dependencies
(require (lib "eopl.ss" "eopl"))
(require parser-tools/lex (prefix-in : parser-tools/lex-sre) parser-tools/yacc)

; Datatypes
(define-datatype program program?
  [prog
   [statements statements?]])

(define-datatype statements statements?
  [single-stmt
   [stmt statement?]]
  [multi-stmts
   [stmt statements?]
   [stmts statement?]])

(define-datatype statement statement?
  [simple-stmt
   [stmt simple-statement?]]
  [compound-stmt
   [stmt compound-statement?]])

(define-datatype simple-statement simple-statement?
  [assign-stmt
   [assign-var identifier?]
   [assign-val expression?]]
  [return-stmt
   [statement return-statement?]]
  [global-stmt
   [variable identifier?]]
  [pass-stmt]
  [break-stmt]
  [continue-stmt])

(define-datatype return-statement return-statement?
  [return-with-value-stmt
   [return-value expression?]]
  [return-without-value-stmt])

(define-datatype compound-statement compound-statement?
  [function-def-with-param-stmt
   [id identifier?]
   [params params?] 
   [statements statement?]]
  [function-def-without-param-stmt
   [id identifier?]
   [statements statement?]]
  [if-stmt
   [condition expression?]
   [on-true statements?]
   [on-false statements?]] ; instead else block
  [for-stmt
   [iterator identifier?]
   [iterating expression?]
   [body statements?]]
  )

(define-datatype param-with-default param-with-default?
  [param
   [id identifier?]
   [expression expression?]])

(define-datatype params params?
  [single-param
   [param-with-defualt param-with-default?]]
  [multi-params
   [rest-params params?]
   [last-param param-with-default?]])

(define-datatype expressions expressions?
  [single-exp
   [exp expression?]]
  [multi-exp
   [rest-exps expressions?]
   [last-exp expression?]])

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
   [compration-op comprasion-expression?]])

(define-datatype comprasion-expression comprasion-expression?
  [comparing-compration-exp
   [sum-op sum-expression?]
   [pairs-op compare-operator-sum-pairs?]]
  [no-compration-exp
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
  ;[power**
   ;[atom-op atom-expression?]
   ;[factor-op factor-expression?]]
  ;[power-nop
   ;[primary-op primary-expression?]]
  )

; required extractors
(define last-sum-of-pairs (lambda (pairs) '()))

; value-of-expression calculation
(define value-of-expression
  (lambda (exp)
    (cases expression exp
      (disjunction-exp (disjunction-op) (value-of-disjunction-expression exp)))))

(define value-of-disjunction-expression
  (lambda (disjunction-exp)
    (cases disjunction-expression disjunction-exp
      (simple-disjunction-exp (conjunction-op)
                              (value-of-conjunction-expression conjunction-op))
      (complex-disjunction-exp (disjunction-op conjunction-op)
                               (or
                                (value-of-disjunction-expression disjunction-op)
                                (value-of-conjunction-expression conjunction-op))))))

(define value-of-conjunction-expression
  (lambda (conjunction-exp)
    (cases conjunction-expression conjunction-exp
      (simple-conjunction-exp (inversion-op)
                              (value-of-inversion-expression inversion-op))
      (complex-conjunction-exp (conjunction-op inversion-op)
                               (and
                                (value-of-conjunction-expression conjunction-op)
                                (value-of-inversion-expression inversion-op))))))

(define value-of-inversion-expression
  (lambda (inversion-exp)
    (cases inversion-expression inversion-exp
      (not-inversion-exp (inversion-op) (not (value-of-inversion-expression inversion-op)))
      (comprasion-inversion-exp (compration-op) (value-of-comprasion-expression compration-op)))))

(define value-of-comprasion-expression
  (lambda (comprasion-exp)
    (cases comprasion-expression comprasion-exp
      (comparing-compration-exp (sum-op pairs-op)
                                (cases compare-operator-sum-pairs pairs-op
                                  (single-operator-sum-pair (pair) (value-of-compare-operator-sum-pair sum-op pair))
                                  (multi-operator-sum-pairs (rest-pairs last-pair)
                                                            (if (value-of-comprasion-expression (comparing-compration-exp sum-op rest-pairs))
                                                                (value-of-compare-operator-sum-pair (last-sum-of-pairs rest-pairs) last-pair)
                                                                #f))))
      (no-compration-exp (sum-op) (value-of-sum-expression sum-op)))))

(define value-of-compare-operator-sum-pair
  (lambda (external-sum pair)
    (cases compare-operator-sum-pair pair
      (eq-sum (sum-op) (= external-sum (value-of-sum-expression sum-op)))
      (lt-sum (sum-op) (< external-sum (value-of-sum-expression sum-op)))
      (gt-sum (sum-op) (> external-sum (value-of-sum-expression sum-op))))))

(define value-of-sum-expression
  (lambda (sum-exp)
    (cases sum-expression sum-exp
      (sum+ (sum-op term-op) (+ (value-of-sum-expression sum-op) (value-of-term-expression term-op)))
      (sum- (sum-op term-op) (- (value-of-sum-expression sum-op) (value-of-term-expression term-op)))
      (sum-nop (term-op) (value-of-term-expression term-op)))))

(define value-of-term-expression
  (lambda (term-exp)
    (cases term-expression term-exp
      (term* (term-op factor-op) (* (value-of-term-expression term-op) (value-of-factor-expression factor-op)))
      (term/ (term-op factor-op) (/ (value-of-term-expression term-op) (value-of-factor-expression factor-op)))
      (term-nop (factor-op) (value-of-factor-expression factor-op)))))

(define value-of-factor-expression
  (lambda (factor-exp)
    (cases factor-expression factor-exp
      (factor+ (factor-op) (* +1 (value-of-factor-expression factor-op)))
      (factor- (factor-op) (* -1 (value-of-factor-expression factor-op)))
      (factor-nop (power-op) (value-of-power-expression power-op)))))

(define value-of-power-expression ;todo
;  (lambda (power-exp)
;    (cases power-expression power-exp
;      (power** (atom-op factor-op) '())
;      (power-nop (primary-op) '())))
  '() )

; lexer
(define main-lexer
           (lexer
            ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUM (string->number lexeme)))
            ((:& (repetition 1 +inf.0 (union (char-range #\a #\z) (char-range #\A #\Z)))
                 (complement (:or (:? "if") (:? "else") (:? "def") (:? "break") (:? "pass") (:? "continue") (:? "return") (:? "global")
                                  (:? "for") (:? "in") (:? "or") (:? "and") (:? "not") (:? "True") (:? "False") (:? "None")))) (token-ID lexeme))
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
(define-tokens a (NUM ID))
(define-empty-tokens b (EOF sc pass break continue = return global def open_par close_par dd o_c_p_d cama
                            if else for in or and not == < > + - * / ** open_q close_q o_c_p True False None))

; parser
(define main-parser
           (parser
            (start Program)
            (end EOF)
            (error void)
            (tokens a b)
           (grammar
            (Program ((Statements) (prog $1)))
            (Statements ((Statement sc) (single-stmt $1))
                        ((Statements Statement sc) (multi-stmts $1 $2)))
            (Statement ((Compound_stmt) (compound-stmt $1))
                       ((Simple_stmt) (simple-stmt $1)))
            (Simple_stmt ((Assignment) (assign-stmt $1))
                         ((Return_stmt) (return-stmt $1))
                         ((Global_stmt) (global-stmt $1))
                         ((pass) (pass-stmt))
                         ((break) (break-stmt))
                         ((continue) (continue-stmt)))
            (Compound_stmt ((Function_def) (function_def $1))
                          ((If_stmt) (if_stmt $1))
                          ((For_stmt) (for_stmt $1)))
            (Assignment ((ID = Expression) (assignment $1 $3)))
            (Return_stmt ((return) (return_void_stmt))
                        ((return Expression) (return_value-stmt $1)))
            (Global_stmt ((global ID) (global_stmt $2)))
            (Function_def ((def ID open_par Params close_par dd Statements) (func_def_par $2 $4 $7))
                         ((def ID o_c_p_d Statements) (func_def_nopar $2 $4)))
            (Params ((Param_with_default) (param_with_default $1))
                   ((Params cama Param_with_default) (params $1 $3)))
            (Param_with_default ((ID = Expression) (param $1 $3)))
            (If_stmt ((if Expression dd Statements Else_block) (if_stmt $2 $4 $5)))
            (Else_block ((else dd Statements) (else_block $3)))
            (For_stmt ((for ID in Expression dd Statements) (for_stmt $2 $4 $6)))
            (Expression ((Disjunction) (expression $1)))
            (Disjunction ((Conjunction) (conjunction $1))
                        ((Disjunction or Conjunction) (dis-or-conj $1 $3)))
            (Conjunction ((Inversion) (inversion $1))
                        ((Conjunction and Inversion) (conj-and-inv $1 $3)))
            (Inversion ((not Inversion) (not-inv $2))
                      ((Comparison) (comparison $1)))
            (Comparison ((Sum Compare_op_Sum_Pairs) (sum_comps $1 $2))
                       ((Sum) (sum $1)))
            (Compare_op_Sum_Pairs ((Compare_op_Sum_Pair) (single_comp $1))
                                 ((Compare_op_Sum_Pairs Compare_op_Sum_Pair) (multi_comp $1 $2)))
            (Compare_op_Sum_Pair ((Eq_sum) (eq_sum $1))
                                ((Lt_sum) (lt_sum $1))
                                ((Gt_sum) (gt_sum $1)))
            (Eq_sum ((== Sum) (eq_sum $2)))
            (Lt_sum ((< Sum) (lt_sum $2)))
            (Gt_sum ((> Sum) (gt_sum $2)))
            (Sum ((Sum + Term) (sum_plus $1 $3))
                ((Sum - Term) (sum_minus $1 $3))
                ((Term) (sum_term $1)))
            (Term ((Term * Factor) (term_mult $1 $3))
                 ((Term / Factor) (term_div $1 $3))
                 ((Factor) (term_factor $1)))
            (Factor ((+ Factor) (factor_plus $2))
                   ((- Factor) (factor_minus $2))
                   ((Power) (factor_power $1)))
            (Power ((Atom ** Factor) (power_atom $1 $3))
                  ((Primary) (power_prim $1)))
            (Primary ((Atom) (prim_atom $1))
                    ((Primary open_q Expression close_q) (prim_exp $1 $3))
                    ((Primary o_c_p) (prim_empt $1))
                    ((Primary open_par Arguments close_par) (prim_args $1 $3)))
            (Arguments ((Expression) (arg_exp $1))
                      ((Arguments cama Expression) (arg_mult_exp $1 $3)))
            (Atom ((ID) (ident $1))
                 ((True) (bool-true))
                 ((False) (bool-false))
                 ((None) (none))
                 ((NUM) (cons $1))
                 ((List) (list $1)))
            (List ((open_q Expressions close_q) (list_exps $2))
                 ((open_q close_q) (empty_list)))
            (Expressions ((Expressions cama Expression) (exp_multi $1 $2))
                        ((Expression) (exp_single $1)))
            )))

;Test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "1+2+ 3 +   4")))
(let ((parser-res (simple-math-parser my-lexer))) parser-res)

