;; (setq lisp-indent-function 'common-lisp-indent-function)
;; (paredit-mode)

;;                              ________
;;                             /_  __/ /_  ___
;;                              / / / __ \/ _ \
;;                             / / / / / /  __/
;;                            /_/ /_/ /_/\___/
;;     __    _________ ____     ________          ____
;;    / /   /  _/ ___// __ \   / ____/ /_  ____ _/ / /__  ____  ____ ____
;;   / /    / / \__ \/ /_/ /  / /   / __ \/ __ `/ / / _ \/ __ \/ __ `/ _ \
;;  / /____/ / ___/ / ____/  / /___/ / / / /_/ / / /  __/ / / / /_/ /  __/
;; /_____/___//____/_/       \____/_/ /_/\__,_/_/_/\___/_/ /_/\__, /\___/
;;                                                           /____/
;;
;; The LISP Challenge
;;
;; Pick your favorite programming language
;; Implement the tiniest possible LISP machine that
;; Bootstraps John Mccarthy'S metacircular evaluator below
;; Winning is defined by lines of code for scripting languages
;; Winning is defined by binary footprint for compiled languages
;;
;; Listed Projects
;;
;; - 512 bytes: https://github.com/jart/sectorlisp
;; - 13 kilobytes: https://t3x.org/klisp/
;; - 47 kilobytes: https://github.com/matp/tiny-lisp
;; - 150 kilobytes: https://github.com/JeffBezanson/femtolisp
;; - Send pull request to be listed here
;;
;; @see LISP From Nothing; Nils M. Holm; Lulu Press, Inc. 2020
;; @see Recursive Functions of Symbolic Expressions and Their
;;      Computation By Machine, Part I; John McCarthy, Massachusetts
;;      Institute of Technology, Cambridge, Mass. April 1960

;; NIL ATOM
;; ABSENCE OF VALUE AND TRUTH
NIL

;; CONS CELL
;; BUILDING BLOCK OF DATA STRUCTURES
(CONS NIL NIL)
(CONS (QUOTE X) (QUOTE Y))

;; REFLECTION
;; EVERYTHING IS AN ATOM OR NOT AN ATOM
(ATOM NIL)
(ATOM (CONS NIL NIL))

;; QUOTING
;; CODE IS DATA AND DATA IS CODE
(QUOTE (CONS NIL NIL))
(CONS (QUOTE CONS) (CONS NIL (CONS NIL NIL)))

;; LOGIC
;; BY WAY OF STRING INTERNING
(EQ (QUOTE A) (QUOTE A))
(EQ (QUOTE T) (QUOTE F))

;; FIND FIRST ATOM IN TREE
;; CORRECT RESULT OF EXPRESSION IS `A`
;; RECURSIVE CONDITIONAL FUNCTION BINDING
((LAMBDA (FF X) (FF X))
 (QUOTE (LAMBDA (X)
          (IF (ATOM X) X
                       (FF (CAR X)))))
 (QUOTE ((A) B C)))

;; LISP IMPLEMENTED IN LISP
;; WITHOUT ANY SUBJECTIVE SYNTACTIC SUGAR
;; RUNS "FIND FIRST ATOM IN TREE" PROGRAM
;; CORRECT RESULT OF EXPRESSION IS STILL `A`
;; REQUIRES CONS CAR CDR QUOTE ATOM EQ LAMBDA IF
;; SIMPLIFIED BUG FIXED VERSION OF JOHN MCCARTHY PAPER
;; NOTE: ((EQ (CAR E) ()) (QUOTE *UNDEFINED)) CAN HELP
;; NOTE: ((EQ (CAR E) (QUOTE LAMBDA)) E) IS NICE
((LAMBDA (ASSOC EVCON PAIRLIS EVLIS APPLY EVAL)
   (EVAL (QUOTE ((LAMBDA (FF X) (FF X))
                 (QUOTE (LAMBDA (X)
                          (COND ((ATOM X) X)
                                ((QUOTE T) (FF (CAR X))))))
                 (QUOTE ((A) B C))))
         ()))
 (QUOTE (LAMBDA (X Y)
          (IF (EQ Y ())           ()
          (IF (EQ X (CAR (CAR Y))) (CDR (CAR Y))
                                  (ASSOC X (CDR Y))))))
 (QUOTE (LAMBDA (C A)
          (IF (EVAL (CAR (CAR C)) A) (EVAL (CAR (CDR (CAR C))) A)
                                     (EVCON (CDR C) A))))
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
             (IF (EQ (CAR E) (QUOTE COND)) (EVCON (CDR E) A)
                             (APPLY (CAR E) (EVLIS (CDR E) A) A)))
             (APPLY (CAR E) (EVLIS (CDR E) A) A))))))
