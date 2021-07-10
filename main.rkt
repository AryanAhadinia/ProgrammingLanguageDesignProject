#lang racket

;Adding required Dependencies
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require (lib "eopl.ss" "eopl"))
;data types
(define-datatype prog prog?
  [program [stmts statements?]]
  )
(define-datatype statements statements?
  [single_statements [stmt statement?]]
  [multi_statements [stmts statements?]
                    [stmt statement?]]
  )
(define-datatype statement statement?
  [compound_stmt [cmp_stmt compound_statement?]]
  [simple_stmt [smp_stmt simple_statement?]]
  )
(define-datatype simple_statement simple_statement?
  [assignment_stmt [assign assignment?]]
  [return_stmt [return return?]]
  [global_stmt [global global?]]
  [pass_stmt]
  [break_stmt]
  [continue_stmt]
  )
;Main Lexer
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
            (whitespace (simple-math-lexer input-port))
            ((eof) (token-EOF))))
;Tokens
(define-tokens a (NUM ID))
(define-empty-tokens b (EOF sc pass break continue = return global def open_par close_par dd o_c_p_d cama
                            if else for in or and not == < > + - * / ** open_q close_q o_c_p True False None
                            ))
;Main Parser
(define main-parser
           (parser
            (start Program)
            (end EOF)
            (error void)
            (tokens a b)
            (grammar
           (Program ((Statements) (program $1)) )
           (Statements ((Statement sc) (single_statements $1))
                       ((Statements Statement sc) (multi_statements $1 $2)))
           (Statement ((Compound_stmt) (compound_stmt $1))
                      ((Simple_stmt) (simple_stmt $1)))
           (Simple_stmt ((Assignment) (assignment_stmt $1))
                        ((Return_stmt) (return_stmt $1))
                        ((Global_stmt) (global_stmt $1))
                        ((pass) (pass_stmt))
                        ((break) (break_stmt))
                        ((continue) (continue_stmt)))
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

