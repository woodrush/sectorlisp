;; EXECLINE: LINE -> STATE, Execute line and return the next state
(QUOTE
  (LAMBDA (LABEL STATEMENT BODY)
    (COND
      ((EQ LABEL (QUOTE let))
       )
      ((EQ LABEL (QUOTE ifzero)
       ))
      ((EQ LABEL (QUOTE print)
       ))
      ((EQ LABEL (QUOTE goto)
       )))))
;; FINDSTATEMENT: LABEL -> STATEMENT, Find the listing for a given label
(QUOTE
  (LAMBDA (LABEL LISTING)
    (COND
      (EQ (CAR (CAR LISTING)) LABEL) (CAR (LISTING))
      ((QUOTE T) (FINDBYLABEL LABEL (CDR LISTING))))))
;; +: INT -> INT, Add 
(QUOTE
  (LAMBDA (N M)
    (COND
      ((EQ NIL M)) N)
      ((QUOTE T) (+ (CONS (QUOTE *) N) (CDR M)))))
;; -: INT -> INT, Subtract
(QUOTE
  (LAMBDA (N M)
    (COND
      ((EQ NIL N) ())
      ((EQ NIL M) N)
      ((QUOTE T) (- (CDR N) (CDR M))))))
;; RESOLVEVAR: VAR/INT -> INT, Resolve the integer value of a variable
(QUOTE
  (LAMBDA (VARNAME VARENV)
    (COND
      ((EQ (ATOM VARNAME) NIL) (VARNAME))
      ((EQ NIL VARENV) ())
      ((EQ VARNAME (CAR (CAR VARENV))) (CAR (CDR VARENV)))
      ((QUOTE T) (RESOLVEVAR VARNAME (CDR VARENV))))))
;; EVALEXPR: EXPR -> INT, Evaluate integer expression
(QUOTE
  (LAMBDA (EXPR)
    (COND
      ((EQ NIL (CDR EXPR)) (RESOLVEVAR (CAR EXPR)))
      ((QUOTE T)
       ((LAMBDA (X OPERAND Y)
          (COND
            ((EQ OPERAND (QUOTE +)) (+ (RESOLVEVAR X) (RESOLVEVAR Y)))
            ((EQ OPERAND (QUOTE -)) (- (RESOLVEVAR X) (RESOLVEVAR Y)))))
        (CAR EXPR) (CAR (CDR EXPR)) (CAR (CDR (CDR EXPR))))))))
;; PRINTINT: INT -> VOID, Print integer value in unary
(QUOTE
  (LAMBDA (N)
    (CONS (PRINT (QUOTE [))
    (CONS (PRINTINTBODY N)
          (PRINT (QUOTE ]))))))
;; PRINTINTBODY: INT -> VOID, Helper function for PRINTINT
(QUOTE
  (LAMBDA (N)
    (COND
      ((EQ NIL N) ())
      ((QUOTE T) (CONS (PRINT (QUOTE *)) (PRINTINTBODY (CDR N)))))))

(10 let n = ())
(20 let m = (a a a a a))
(30 ifzero m then 80)
(40 print n)
(50 let n = n + (a))
(60 let m = m - 1)
(70 goto 30)
