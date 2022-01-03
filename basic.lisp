;; (10 let n = (a a a a a))
;; (20 let m = ())
;; (30 ifzero n then 80)
;; (40 let m = m + (a))
;; (50 let n = n - (a))
;; (60 print m)
;; (70 goto 30)

;; (50 let n = (* * * *) + (*))
;; (60 print n)
;; (61 ifzero n then 90)
;; (62 let n = ())
;; (65 print (*))
;; (70 ifzero n then 90)
;; (80 print n)
;; (90 print (*))
;; (100 print (* *))
((LAMBDA (EXECLINE CONSINITSTATE CONSSTATE FINDLABELLISTING + - RESOLVEVAR VARENVPREPEND EVALEXPR PRINTINT)
  ;; (RESOLVEVAR
  ;;   (QUOTE N)
  ;;   (VARENVPREPEND (QUOTE M) (QUOTE (* * * *)) (VARENVPREPEND (QUOTE N) (QUOTE (* * *)) ())))

  ;; (EVALEXPR (QUOTE ((* * *) - (* *))))
  ;; (- (QUOTE (* * * * *)) (QUOTE (* *)))

  ;; (EXECLINE (CONSINITSTATE
  ;;   (QUOTE (
  ;;     (60 print (* * *))
  ;;   ))))

  ((LAMBDA (STATE LOOP) (LOOP STATE LOOP))
    (CONSINITSTATE
      (QUOTE
        (
          (10 let n = (* * * * *))
          (20 let m = ())
          (30 ifzero n then 80)
          (40 let m = m + (a))
          (50 let n = n - (a))
          (60 print m)
          (70 goto 30)
         )))
    (QUOTE
      (LAMBDA (STATE LOOP)
        (COND
          ((EQ NIL (CAR (CDR (CDR STATE)))) STATE)
          ((QUOTE T) (LOOP (EXECLINE STATE) LOOP))))))
 )
 ;; EXECLINE: STATE -> STATE: Execute line and return the next state
 (QUOTE
   (LAMBDA (STATE)
     ((LAMBDA (CURSTATEMENT VARENV FULLLISTING CURLISTING)
        ((LAMBDA (LABEL STATEMENT BODY)
           (COND
             ((EQ STATEMENT (QUOTE let))
              (CONSSTATE
                ((LAMBDA (VARNAME EXPR)
                   (VARENVPREPEND VARNAME (EVALEXPR EXPR VARENV) VARENV))
                 (CAR BODY) (CDR (CDR BODY)))
                FULLLISTING
                (CDR CURLISTING)))
             ((EQ STATEMENT (QUOTE ifzero))
              ((LAMBDA (N DESTLABEL)
                 (CONSSTATE
                   VARENV
                   FULLLISTING
                   (COND
                     ((EQ NIL N)
                      (FINDLABELLISTING DESTLABEL FULLLISTING))
                     ((QUOTE T)
                      (CDR CURLISTING)))))
               (RESOLVEVAR (CAR BODY) VARENV) (CAR (CDR (CDR BODY)))))
             ((EQ STATEMENT (QUOTE print))
              (CDR (CONS (PRINTINT (EVALEXPR BODY VARENV))
                         (CONSSTATE
                           VARENV
                           FULLLISTING
                           (CDR CURLISTING)))))
             ((EQ STATEMENT (QUOTE goto))
              (CONSSTATE
                VARENV
                FULLLISTING
                (FINDLABELLISTING (CAR BODY) FULLLISTING)))))
         (CAR CURSTATEMENT)
         (CAR (CDR CURSTATEMENT))
         (CDR (CDR CURSTATEMENT))))
      (CAR (CAR (CDR (CDR STATE))))
      (CAR STATE)
      (CAR (CDR STATE))
      (CAR (CDR (CDR STATE))))))
 
 
 ;; CONSINITSTATE: FULLLISTING -> STATE
 (QUOTE
   (LAMBDA (FULLLISTING)
     (CONS () (CONS FULLLISTING (CONS FULLLISTING ())))))
 
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
       ((EQ NIL CURLISTING) NIL)
       ((EQ (CAR (CAR CURLISTING)) LABEL) CURLISTING)
       ((QUOTE T) (FINDLABELLISTING LABEL (CDR CURLISTING))))))
 
 ;; +: INT -> INT: Add
 (QUOTE
   (LAMBDA (N M)
     (COND
       ((EQ NIL M) N)
       ((QUOTE T) (+ (CONS (QUOTE *) N) (CDR M))))))
 
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
       ((EQ (ATOM VARNAME) NIL) VARNAME) ;; When the input is an integer
       ((EQ NIL VARENV) ())
       ((EQ VARNAME (CAR (CAR VARENV))) (CDR (CAR VARENV)))
       ((QUOTE T) (RESOLVEVAR VARNAME (CDR VARENV))))))
 
 ;; VARENVPREPEND: VARNAME, INT, VARENV -> VARENV: Update the variable value in the varenv
 (QUOTE
   (LAMBDA (VARNAME N VARENV)
     (CONS (CONS VARNAME N) VARENV)))
 
 ;; EVALEXPR: EXPR, VARENV -> INT: Evaluate integer expressions.
 ;; EXPR is a list even if the input is a single variable or an integer literal.
 (QUOTE
   (LAMBDA (EXPR VARENV)
     (COND
       ((EQ NIL (CDR EXPR)) (RESOLVEVAR (CAR EXPR) VARENV))
       ((QUOTE T)
        ((LAMBDA (X OPERAND Y)
           (COND
             ((EQ OPERAND (QUOTE +)) (+ (RESOLVEVAR X VARENV) (RESOLVEVAR Y VARENV)))
             ((EQ OPERAND (QUOTE -)) (- (RESOLVEVAR X VARENV) (RESOLVEVAR Y VARENV)))))
         (CAR EXPR) (CAR (CDR EXPR)) (CAR (CDR (CDR EXPR))))))))
 
 ;; PRINTINT: INT -> VOID: Print integer value in unary
 (QUOTE
   (LAMBDA (N)
     ((LAMBDA (PRINTINTBODY)
        (CONS (PRINT (QUOTE [))
        (CONS (PRINTINTBODY N)
              (PRINT (QUOTE ])))))
      ;; PRINTINTBODY: INT -> VOID: Helper function for PRINTINT
      (QUOTE
        (LAMBDA (N)
          (COND
            ((EQ NIL N) ())
            ((QUOTE T) (CONS (PRINT (QUOTE *)) (PRINTINTBODY (CDR N))))))))))



 )
