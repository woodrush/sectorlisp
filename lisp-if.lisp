((LAMBDA (ASSOC EVIF PAIRLIS EVLIS APPLY EVAL)
   (EVAL (QUOTE ((LAMBDA (FF X) (FF X))
                 (QUOTE (LAMBDA (X)
                          (IF (ATOM X) X
                                       (FF (CAR X)))))
                 (QUOTE ((A) B C))))
         ()))
 (QUOTE (LAMBDA (X Y)
          (IF (EQ Y ())           ()
          (IF (EQ X (CAR (CAR Y))) (CDR (CAR Y))
                                  (ASSOC X (CDR Y))))))
 (QUOTE (LAMBDA (C A)
          (IF (EVAL (CAR C) A) (EVAL (CAR (CDR C)) A)
                               (EVAL (CAR (CDR (CDR C))) A))))
 (QUOTE (LAMBDA (X Y A)
          (IF (EQ X ()) A
                        (CONS (CONS (CAR X) (CAR Y))
                              (PAIRLIS (CDR X) (CDR Y) A)))))
 (QUOTE (LAMBDA (M A)
          (IF (EQ M ()) ()
                        (CONS (EVAL (CAR M) A)
                              (EVLIS (CDR M) A)))))
 (QUOTE (LAMBDA (FN X A)
          (IF (ATOM FN)
              (IF (EQ FN (QUOTE CAR))  (CAR  (CAR X))
              (IF (EQ FN (QUOTE CDR))  (CDR  (CAR X))
              (IF (EQ FN (QUOTE ATOM)) (ATOM (CAR X))
              (IF (EQ FN (QUOTE CONS)) (CONS (CAR X) (CAR (CDR X)))
              (IF (EQ FN (QUOTE EQ))   (EQ   (CAR X) (CAR (CDR X)))
                                       (APPLY (EVAL FN A) X A))))))
          (IF (EQ (CAR FN) (QUOTE LAMBDA))
              (EVAL (CAR (CDR (CDR FN)))
                    (PAIRLIS (CAR (CDR FN)) X A))
              ()))))
 (QUOTE (LAMBDA (E A)
          (IF (ATOM E) (ASSOC E A)
          (IF (ATOM (CAR E))
             (IF (EQ (CAR E) (QUOTE QUOTE)) (CAR (CDR E))
             (IF (EQ (CAR E) (QUOTE IF)) (EVIF (CDR E) A)
                             (APPLY (CAR E) (EVLIS (CDR E) A) A)))
             (APPLY (CAR E) (EVLIS (CDR E) A) A))))))
