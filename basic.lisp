((LAMBDA (BASICINTERPRETER)
   (BASICINTERPRETER
     (QUOTE (
       (10 let n = (o o o o o))
       (20 let m = ())
       (30 ifzero n then 80)
       (40 let m = m + (a))
       (50 let n = n - (a))
       (60 print m)
       (70 goto 30)
     ))))

 (QUOTE
   (LAMBDA (FULLLISTING)
     ((LAMBDA (EXECLINE CONSINITSTATE CONSSTATE FINDLABELLISTING + -
               RESOLVEVAR VARENVPREPEND EVALEXPR PRINTINT
               PRINT ISNOTSECTORLISP APPEND)
       ((LAMBDA (STATE LOOP) (LOOP STATE LOOP))
         (CONSINITSTATE FULLLISTING)
         (QUOTE
           (LAMBDA (STATE LOOP)
             (COND
               ((EQ NIL (CAR (CDR (CDR STATE))))
                (COND
                  (ISNOTSECTORLISP NIL)
                  ((QUOTE T) (CAR (CDR (CDR (CDR STATE)))))))
               ((QUOTE T) (LOOP (EXECLINE STATE) LOOP)))))))

      ;; EXECLINE: STATE -> STATE: Execute line and return the next state
      (QUOTE
        (LAMBDA (STATE)
          ((LAMBDA (CURSTATEMENT VARENV FULLLISTING CURLISTING OUTPUT)
             ((LAMBDA (LABEL STATEMENT BODY)
                (COND
                  ((EQ STATEMENT (QUOTE let))
                   (CONSSTATE
                     ((LAMBDA (VARNAME EXPR)
                        (VARENVPREPEND VARNAME (EVALEXPR EXPR VARENV) VARENV))
                      (CAR BODY) (CDR (CDR BODY)))
                     FULLLISTING
                     (CDR CURLISTING)
                     OUTPUT))
                  ((EQ STATEMENT (QUOTE ifzero))
                   ((LAMBDA (N DESTLABEL)
                      (CONSSTATE
                        VARENV
                        FULLLISTING
                        (COND
                          ((EQ NIL N)
                           (FINDLABELLISTING DESTLABEL FULLLISTING))
                          ((QUOTE T)
                           (CDR CURLISTING)))
                        OUTPUT))
                    (RESOLVEVAR (CAR BODY) VARENV) (CAR (CDR (CDR BODY)))))
                  ((EQ STATEMENT (QUOTE print))
                   ((LAMBDA (NEWOUTPUT)
                      (CONSSTATE
                        VARENV
                        FULLLISTING
                        (CDR CURLISTING)
                        (APPEND OUTPUT NEWOUTPUT)))
                    (PRINTINT (EVALEXPR BODY VARENV))))
                  ((EQ STATEMENT (QUOTE goto))
                   (CONSSTATE
                     VARENV
                     FULLLISTING
                     (FINDLABELLISTING (CAR BODY) FULLLISTING)
                     OUTPUT))))
              (CAR CURSTATEMENT)
              (CAR (CDR CURSTATEMENT))
              (CDR (CDR CURSTATEMENT))))
           (CAR (CAR (CDR (CDR STATE))))
           (CAR STATE)
           (CAR (CDR STATE))
           (CAR (CDR (CDR STATE)))
           (CAR (CDR (CDR (CDR STATE)))))))

      ;; CONSINITSTATE: FULLLISTING -> STATE
      (QUOTE
        (LAMBDA (FULLLISTING)
          (CONS () (CONS FULLLISTING (CONS FULLLISTING (CONS () ()))))))

      ;; CONSSTATE: VARENV, FULLLISTING, CURLISTING -> STATE
      (QUOTE
        (LAMBDA (VARENV FULLLISTING CURLISTING OUTPUT)
          (CONS VARENV (CONS FULLLISTING (CONS CURLISTING (CONS OUTPUT ()))))))

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
            ((QUOTE T) (+ (CONS (QUOTE o) N) (CDR M))))))

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
             (CONS (PRINT (QUOTE ]))
                   NIL))))
           ;; PRINTINTBODY: INT -> VOID: Helper function for PRINTINT
           (QUOTE
             (LAMBDA (N)
               (COND
                 ((EQ NIL N) ())
                 ((QUOTE T) (CONS (PRINT (QUOTE o)) (PRINTINTBODY (CDR N))))))))))

     ;; PRINT: X -> X: For compatibility with the original SectorLISP
     (QUOTE
       (LAMBDA (X) X))

     ;; ISSECTORLISP: Check if PRINT is a reserved keyword
     ((LAMBDA (PRINT)
        (PRINT))
      (QUOTE (LAMBDA () NIL)))

     ;; APPEND
     (QUOTE
       (LAMBDA (L ITEM)
         (COND
           ((EQ NIL L) ITEM)
           ((QUOTE T) (CONS (CAR L) (APPEND (CDR L) ITEM))))))))))
