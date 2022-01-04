((LAMBDA (BASICINTERPRETER)
   (BASICINTERPRETER
     (QUOTE (
       (10   let n_max = (o o o o o o o o o o
                          o o o o o          )     )
       (20   let i = (o)                           )
       (30   if (n_max <= i) then 170              )
       (40       let i = i + (o)                   )
       (50       let j = (o o)                     )
       (70       if (i <= j) then 120              )
       (80           let r = i % j                 )
       (90           if (r <= ()) then 30          )
       (100          let j = j + (o)               )
       (110      goto 70                           )
       (120      print i                           )
       (130  goto 30                               )
     ))))

 (QUOTE
   (LAMBDA (FULLLISTING)
     ((LAMBDA (EXECLINE CONSINITSTATE CONSSTATE FINDLABELLISTING + - % <=
               RESOLVEVAR VARENVPREPEND EVALEXPR PRINTINT
               PRINT HASPRINT APPEND)
       ((LAMBDA (STATE LOOP) (LOOP STATE LOOP))
         (CONSINITSTATE FULLLISTING)
         (QUOTE
           (LAMBDA (STATE LOOP)
             (COND
               ((EQ NIL (CAR (CDR (CDR STATE))))
                (COND
                  (HASPRINT NIL)
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
                  ((EQ STATEMENT (QUOTE if))
                   ((LAMBDA (N DESTLABEL)
                      (CONSSTATE
                        VARENV
                        FULLLISTING
                        (COND
                          ((EQ NIL N)
                           (CDR CURLISTING))
                          ((QUOTE T)
                           (FINDLABELLISTING DESTLABEL FULLLISTING)))
                        OUTPUT))
                    (EVALEXPR (CAR BODY) VARENV) (CAR (CDR (CDR BODY)))))
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

      ;; %: INT -> INT: Mod
      (QUOTE
        (LAMBDA (N M)
          (COND
            ((<= N (- M (QUOTE (o)))) N)
            ((QUOTE T) (% (- N M) M)))))

      ;; <=: INT -> INT: Less than
      (QUOTE
        (LAMBDA (N M)
          (COND
            ((EQ NIL (- N M)) (QUOTE (o)))
            ((QUOTE T) NIL))))

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
                  ((EQ OPERAND (QUOTE +)) (+ X Y))
                  ((EQ OPERAND (QUOTE -)) (- X Y))
                  ((EQ OPERAND (QUOTE %)) (% X Y))
                  ((EQ OPERAND (QUOTE <=)) (<= X Y))))
              (RESOLVEVAR (CAR EXPR) VARENV)
              (CAR (CDR EXPR))
              (RESOLVEVAR (CAR (CDR (CDR EXPR))) VARENV))))))

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

     ;; HASPRINT: Check if PRINT is a reserved keyword
     ((LAMBDA (PRINT)
        (PRINT))
      (QUOTE (LAMBDA () NIL)))

     ;; APPEND
     (QUOTE
       (LAMBDA (L ITEM)
         (COND
           ((EQ NIL L) ITEM)
           ((QUOTE T) (CONS (CAR L) (APPEND (CDR L) ITEM))))))))))
