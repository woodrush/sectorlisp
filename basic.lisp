(10 let n = (a a a a a))
(20 let m = ())
(30 ifzero n then 80)
(40 let m = m + (a))
(50 let n = n - (a))
(60 print m)
(70 goto 30)

;; EXECLINE: ... -> STATE: Execute line and return the next state
(QUOTE
  (LAMBDA (LABEL STATEMENT BODY VARENV FULLLISTING CURLISTING)
    (COND
      ((EQ STATEMENT (QUOTE let))
       (CONSSTATE
         ((LAMBDA (VARNAME EXPR)
            (VARNAMEPREPEND VARNAME (EVALEXPR EXPR) VARENV))
          (CAR BODY) (CDR (CDR BODY)))
         FULLLISTING
         (CDR CURLISTING)))
      ((EQ STATEMENT (QUOTE ifzero)
       ((LAMBDA (N DESTLABEL)
          (COND
            ((EQ NIL N)
             (CONSSTATE
               VARENV
               FULLLISTING
               (FINDLABELLISTING LABEL FULLLISTING)))
            ((QUOTE T)
             (CONSSTATE
               VARENV
               FULLLISTING
               (CDR CURLISTING)))))
        (RESOLVEVAR (CAR BODY)) (CAR (CDR (CDR BODY))))))
      ((EQ STATEMENT (QUOTE print))
       (CDR (CONS (PRINTINT (EVALEXPR BODY))
                  (CONSSTATE
                    VARENV
                    FULLLISTING
                    (CDR CURLISTING)))))
      ((EQ STATEMENT (QUOTE goto)
       (CONSSTATE
         VARENV
         FULLLISTING
         (FINDLABELLISTING (CAR BODY) FULLLISTING)))))))

;; CONSSTATE: VARENV, FULLLISTING, CURLISTING -> STATE
(QUOTE
  (LAMBDA (VARENV FULLLISTING CURLISTING)
    (CONS VARENV (CONS FULLLISTING (CONS CURLISTING ())))))

;; FINDLABELLISTING: LABEL, FULLLISTING -> CURLISTING:
;; Find the listing for a given label,
;; and return the next listing state
(QUOTE
  (LAMBDA (LABEL CURLISTING)
    (COND
      (EQ (CAR (CAR CURLISTING)) LABEL) (CURLISTING))
      ((QUOTE T) (FINDBYLABEL LABEL (CDR CURLISTING))))))

;; +: INT -> INT: Add
(QUOTE
  (LAMBDA (N M)
    (COND
      ((EQ NIL M)) N)
      ((QUOTE T) (+ (CONS (QUOTE *) N) (CDR M)))))

;; -: INT -> INT: Subtract
(QUOTE
  (LAMBDA (N M)
    (COND
      ((EQ NIL N) ())
      ((EQ NIL M) N)
      ((QUOTE T) (- (CDR N) (CDR M))))))

;; RESOLVEVAR: VAR/INT, VARENV -> INT: Resolve the integer value of a variable
(QUOTE
  (LAMBDA (VARNAME VARENV)
    (COND
      ((EQ (ATOM VARNAME) NIL) VARNAME)
      ((EQ NIL VARENV) ())
      ((EQ VARNAME (CAR (CAR VARENV))) (CAR (CDR VARENV)))
      ((QUOTE T) (RESOLVEVAR VARNAME (CDR VARENV))))))

;; VARENVPREPEND: VARNAME, INT, VARENV -> VARENV: Update the variable value in the varenv
(QUOTE
  (LAMBDA (VARNAME N VARENV)
    (CONS (CONS VARNAME N) VARENV)))

;; EVALEXPR: EXPR -> INT: Evaluate integer expressions
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

;; PRINTINT: INT -> VOID: Print integer value in unary
(QUOTE
  (LAMBDA (N)
    (CONS (PRINT (QUOTE [))
    (CONS (PRINTINTBODY N)
          (PRINT (QUOTE ]))))))

;; PRINTINTBODY: INT -> VOID: Helper function for PRINTINT
(QUOTE
  (LAMBDA (N)
    (COND
      ((EQ NIL N) ())
      ((QUOTE T) (CONS (PRINT (QUOTE *)) (PRINTINTBODY (CDR N)))))))




((LAMBDA (PRINTINT PRINTINTBODY)
   (PRINTINT (QUOTE (* * * * *))))
  (QUOTE
    (LAMBDA (N)
      (CONS (PRINT (QUOTE [))
      (CONS (PRINTINTBODY N)
            (PRINT (QUOTE ]))))))
  (QUOTE
    (LAMBDA (N)
      (COND
        ((EQ NIL N) ())
        ((QUOTE T) (CONS (PRINT (QUOTE *)) (PRINTINTBODY (CDR N))))))))
