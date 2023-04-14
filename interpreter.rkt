#lang racket
;; Audrey Zhu, Fletcher Li, Zongbo Zhang

(require "functionParser.rkt")

;; INTERPRETER

; takes a file name and interprets the program written on that file
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (k)
       (evaluate (parser filename) (createstate) k)))))

; finds and runs the main function
(define runmain
  (lambda (state return throw)
    (if (lookup 'main state)
        (funcall (lookup 'main state) '() state throw)
        (error 'noreturn "No value returned"))))

; evaluate a parse tree and return the final state
(define evaluate
  (lambda (tree state return)
    (if (null? tree)
        (runmain state return (lambda (v e)
                                (if (number? e)
                                    (error 'thrownerror (number->string e))
                                    (error 'thrownerror e))))
        (evaluate (cdr tree) (M_state (car tree) state return) return))))

;; HELPER FUNCTIONS

(define operator car)
      
(define leftop cadr)

(define rightop caddr)

(define else cadddr)

(define param cddr)

;; STATE HELPER FUNCTIONS

; creates an empty state with one layer
(define createstate (lambda () '(())))

; returns the top layer of the state
(define toplayer car)

; returns all layers except the top layer of the state
(define restof cdr)

; retrieves the name of the first name-value pair in the top layer of the state
(define firstname
  (lambda (state)
    (if (null? (toplayer state))
        #f
        (car (car (toplayer state))))))

; retrieves the box of the value of the first name-value pair in the top layer of the state,
; or #t if the first name hasn't been initialized
(define firstval
  (lambda (state)
    (cond
      ((null? (cdr (car (toplayer state)))) #t)
      (else                                 (cadr (car (toplayer state)))))))

; adds a name (no value) to the state
(define addname
  (lambda (name state)
    (cons (cons (list name) (toplayer state)) (restof state))))

; adds a name-value pair to the state
(define addbinding
  (lambda (name value state)
    (cons (cons (list name (box value)) (toplayer state)) (restof state))))

; binds a value to an existing name
(define assign
  (lambda (name value state)
    (assign-cpt name value state (lambda (v) v))))

(define assign-cpt
  (lambda (name value state return)
    (cond
      ((null? state)                (error 'novar "Variable Not Declared"))
      ((null? (toplayer state))     (assign-cpt name
                                                value
                                                (restof state)
                                                (lambda (v) (return (cons (toplayer state) v)))))
      ((eq? (firstname state) name) (begin (set-box! (firstval state) value) (return state)))
      (else                         (assign-cpt name
                                                value
                                                (cons (cdr (toplayer state)) (restof state))
                                                (lambda (v)
                                                  (return (cons (cons (car (toplayer state))
                                                                      (toplayer v))
                                                                (restof v)))))))))

; removes a name-value pair from the state
(define removebinding
  (lambda (name state)
    (removebinding-cpt name state (lambda (v) v))))

(define removebinding-cpt
  (lambda (name state return)
    (cond
      ((null? state)            (return state))
      ((null? (toplayer state)) (removebinding-cpt name
                                                   (restof state)
                                                   (lambda (v) (return (cons (toplayer state) v)))))
      (else                      (return (cons (removebinding-layer name
                                                                    (toplayer state)
                                                                    (lambda (v) v))
                                               (restof state)))))))

; helper function that searches through an individual layer of the state
(define removebinding-layer
  (lambda (name layer return)
    (cond
      ((null? layer)                (return layer))
      ((eq? (car (car layer)) name) (return (cdr layer)))
      (else                         (removebinding-layer name
                                                         (cdr layer)
                                                         (lambda (v)
                                                           (return (cons (car layer) v))))))))

; takes a name and a state and returns
; - #f if the name is not in the state
; - #t if the name is in the state but has no value bound to it
; - the value bound to the name otherwise
(define lookup
  (lambda (name state)
    (cond
      ; #f if the variable has not been declared, #t if it has been declared but not initialized
      ((null? state)                #f)
      ((eq? (firstname state) name) (unbox (firstval state)))
      ((null? (toplayer state))     (lookup name (restof state)))
      (else                         (lookup name (cons (cdr (toplayer state)) (restof state)))))))

; takes the output of lookup and returns the variable's value or the appropriate error
(define getvar
  (lambda (v)
    (cond
      ((eq? v #f) (error 'novar "Variable Not Declared"))
      ((eq? v #t) (error 'noval "Variable Not Initalized"))
      (else       v))))

; adds an empty new layer to the top of the state
(define addlayer
  (lambda (state)
    (cons '() state)))

; removes the top layer of the state
(define removelayer restof)

;; FUNCTION-RELATED HELPER FUNCTIONS

; takes a function definition and a state and returns the function's closure
(define makeclosure
  (lambda (func state)
    (list (getfuncname func) (getformparams func) (getbod func))))

; retrieves a function's name from its definition or its closure
(define getfuncname car)

; retrieves a function's formal parameters from its definition or its closure
(define getformparams cadr)

; retrieves a function's body from its definition or its closure
(define getbod caddr)

; takes a function closure and a state and returns the layer of the state where the function resides
(define getfuncstate
  (lambda (closure state)
    (getfuncstate-cpt closure state (lambda (v) v))))

(define getfuncstate-cpt
  (lambda (closure state return)
    (cond
      ((null? state)                                 (error 'nofunc "Function not defined"))
      ((eq? (firstname state) (getfuncname closure)) (return state))
      ((null? (toplayer state))                      (getfuncstate-cpt closure
                                                                       (removelayer state)
                                                                       (lambda (v) v)))
      (else                                          (getfuncstate-cpt closure
                                                                       (cons (cdr (toplayer state))
                                                                             (restof state))
                                                                       (lambda (v)
                                                                         (return
                                                                          (cons
                                                                           (cons
                                                                            (car (toplayer state))
                                                                            (toplayer v))
                                                                           (restof v)))))))))

; takes a set of actual parameters, formal parameters, function state, and current state, and binds
; the actual parameters to the current parameters
(define bindparams
  (lambda (aparams fparams fstate cstate throw)
    (cond
      ((null? fparams) fstate)
      ((null? aparams) ('error 'missparams "Missing at least one input"))
      (else            (bindparams (cdr aparams)
                                   (cdr fparams)
                                   (addbinding (car fparams)
                                               (M_val (car aparams) cstate throw)
                                               fstate)
                                   cstate
                                   throw)))))

;; MAPPINGS

; maps an expression to a numerical value
(define M_val
  (lambda (expr state throw)
    (M_val-cpt expr state (lambda (v) v) throw)))
    
(define M_val-cpt
  (lambda (expr state return throw)
    (cond
      ((or (number? expr) (eq? expr 'true) (eq? expr 'false)) ; atomic value
       (return expr))
      ((symbol? expr) (return (getvar (lookup expr state))))  ; variable
      ((eq? (operator expr) '+)                               ; addition
       (M_val-cpt (leftop expr)
                  state
                  (lambda (v1) (M_val-cpt (rightop expr)
                                          state
                                          (lambda (v2) (return (+ v1 v2)))
                                          throw))
                  throw))
      ((and (eq? (operator expr) '-) (null? (cddr expr)))     ; opposite
       (M_val-cpt (leftop expr) state (lambda (v) (return (* -1 v))) throw))
      ((eq? (operator expr) '-)                               ; subtraction
       (M_val-cpt (leftop expr)
                  state
                  (lambda (v1) (M_val-cpt (rightop expr)
                                          state
                                          (lambda (v2) (return (- v1 v2)))
                                          throw))
                  throw))
      ((eq? (operator expr) '*)                               ; multiplication
       (M_val-cpt (leftop expr)
                  state
                  (lambda (v1) (M_val-cpt (rightop expr)
                                          state
                                          (lambda (v2) (return (* v1 v2)))
                                          throw))
                  throw))
      ((eq? (operator expr) '/)                               ; division
       (M_val-cpt (leftop expr)
                  state
                  (lambda (v1)
                    (M_val-cpt (rightop expr) state (lambda (v2) (return (quotient v1 v2))) throw))
                  throw))
      ((eq? (operator expr) '%)                               ; modulo
       (M_val-cpt (leftop expr)
                  state
                  (lambda (v1)
                    (M_val-cpt (rightop expr) state (lambda (v2) (return (remainder v1 v2))) throw))
                  throw))
      ((eq? #t (M_bool expr state throw)) (return 'true))     ; true
      ((eq? #f (M_bool expr state throw)) (return 'false))    ; false
      ((eq? 'funcall (operator expr)) (return (funcall (lookup (leftop expr) state) ; function call
                                                       (param expr)
                                                       state
                                                       throw))) 
      (else (error 'unknownop "Bad Operator"))))) ; error

; M_val function for processing function calls
; takes a function name and a list of actual parameters and returns the function's return value
(define funcall
  (lambda (closure params state throw)
    (M_state (cons 'begin (getbod closure))
             (bindparams params
                         (getformparams closure)
                         (addlayer (getfuncstate closure state))
                         state
                         throw)
             (lambda (v) v))))

; maps an expression to a boolean value
(define M_bool
  (lambda (expr state throw)
    (M_bool-cpt expr state (lambda (v) v) throw)))

(define M_bool-cpt
  (lambda (expr state return throw)
    (cond
      ((or (eq? expr #t) (eq? expr 'true))  (return #t))                                        ; #t
      ((or (eq? expr #f) (eq? expr 'false)) (return #f))                                        ; #f
      ((symbol? expr)                       (return (getvar (lookup expr state))))              ; var
      ((eq? (operator expr) '<)             (return (< (M_val (leftop expr) state throw)        ; <
                                                       (M_val (rightop expr) state throw))))
      ((eq? (operator expr) '>)             (return (> (M_val (leftop expr) state throw)        ; >
                                                       (M_val (rightop expr) state throw))))
      ((eq? (operator expr) '==)            (return (eq? (M_val (leftop expr) state throw)      ; =
                                                         (M_val (rightop expr) state throw))))
      ((eq? (operator expr) '<=)            (return (<= (M_val (leftop expr) state throw)       ; <=
                                                        (M_val (rightop expr) state throw))))
      ((eq? (operator expr) '>=)            (return (>= (M_val (leftop expr) state throw)       ; >=
                                                        (M_val (rightop expr) state throw))))
      ((eq? (operator expr) '!=)            (return (not (eq? (M_val (leftop expr) state throw) ; !=
                                                              (M_val (rightop expr) state throw)))))
      ((eq? (operator expr) '!)             (M_bool-cpt (leftop expr)                           ; !
                                                        state
                                                        (lambda (v)
                                                          (return (not (M_bool-cpt v
                                                                                   state
                                                                                   return
                                                                                   throw))))
                                                        throw))
      ((eq? (operator expr) '&&)            (M_bool-cpt (leftop expr)                           ; and
                                                        state
                                                        (lambda (v1)
                                                          (M_bool-cpt (rightop expr)
                                                                      state
                                                                      (lambda (v2)
                                                                        (return
                                                                         (and (M_bool-cpt v1
                                                                                          state
                                                                                          return
                                                                                          throw)
                                                                              (M_bool-cpt v2
                                                                                          state
                                                                                          return
                                                                                          throw))))
                                                                      throw))
                                                        throw))
      ((eq? (operator expr) '||)            (M_bool-cpt (leftop expr)                           ; or
                                                        state
                                                        (lambda (v1)
                                                          (M_bool-cpt (rightop expr)
                                                                      state
                                                                      (lambda (v2)
                                                                        (return
                                                                         (or (M_bool-cpt v1
                                                                                         state
                                                                                         return
                                                                                         throw)
                                                                             (M_bool-cpt v2
                                                                                         state
                                                                                         return
                                                                                         throw))))
                                                                      throw))
                                                        throw))
      ((eq? 'funcall (operator expr))       (return (funcall (lookup (leftop expr) state)       ; fun
                                                             (param expr)
                                                             state
                                                             throw)))
      (else                                 (return -1)))))

;; M_STATE FUNCTIONS

; takes an expression and a state and returns a new state
(define M_state
  (lambda (expr state return)
    (M_state-cpt expr
                 state
                 return
                 (lambda (v) v)
                 (lambda (v) (error 'badcontinue "Invalid Continue"))
                 (lambda (v) (error 'badbreak "Invalid Break"))
                 (lambda (v e)
                   (if (number? e)
                       (error 'thrownerror (number->string e))
                       (error 'thrownerror e))))))

(define M_state-cpt
  (lambda (expr state return next continue break throw)
    (cond
      ((null? expr)       (next state))
      ((not (pair? expr)) (next state))                                            ; not a statement
      ((and (eq? (operator expr) 'var) (null? (cddr expr)))                        ; declaration
       (next (addname (leftop expr) (removebinding (leftop expr) state))))
      ((eq? (operator expr) 'var)                                                  ; var x = y
       (next (addbinding (leftop expr) (M_val (rightop expr) state throw) state)))
      ((and (eq? (operator expr) '=) (eq? #f (lookup (leftop expr) state)))        ; no variable
       (error 'novar "Variable Not Declared"))                          
      ((eq? (operator expr) '=) (next (assign (leftop expr)                        ; assignment
                                              (M_val (rightop expr) state throw)
                                              state)))
      ((and (or (eq? (operator expr) 'if) (eq? (operator expr) 'while))            ; error: condition
            (number? (M_bool (leftop expr) state throw)))
       (error 'badcon "Bad Condition"))
      ((and (eq? (operator expr) 'if) (M_bool (leftop expr) state throw))          ; if: then stmt
       (M_state-cpt (leftop expr)
                    state
                    return
                    (lambda (v) (M_state-cpt (rightop expr) v return next continue break throw))
                    continue
                    break
                    throw)) ;
      ((and (eq? (operator expr) 'if) (not (null? (cdddr expr))))                  ; if: else stmt
       (M_state-cpt (leftop expr)
                    state
                    return
                    (lambda (v) (M_state-cpt (else expr) v return next continue break throw))
                    continue
                    break
                    throw))
      ((eq? (operator expr) 'if)       (M_state-cpt (leftop expr)                  ; if: side effect
                                                    state
                                                    return
                                                    next
                                                    continue
                                                    break
                                                    throw))
      ((eq? (operator expr) 'while)    (whileloop (leftop expr)                    ; while
                                                  (rightop expr)
                                                  state
                                                  return
                                                  next
                                                  continue
                                                  (lambda (v) (next (removelayer v)))
                                                  throw))
      ((eq? (operator expr) 'begin)    (block (restof expr)                        ; statement block
                                              (addlayer state)
                                              return
                                              (lambda (v) (next (removelayer v)))
                                              continue
                                              break
                                              throw))
      ((eq? (operator expr) 'try)      (tcf (leftop expr)                          ; try-catch-cont
                                            (rightop expr)
                                            (cadddr expr)
                                            state
                                            return
                                            next
                                            continue
                                            break
                                            throw))
      ((eq? (operator expr) 'return)   (return (M_val (leftop expr) state throw)))  ; return
      ((eq? (operator expr) 'continue) (continue state))                            ; continue
      ((eq? (operator expr) 'break)    (break state))                               ; break
      ((eq? (operator expr) 'throw)    (throw state (leftop expr)))                 ; throw
      ((eq? (operator expr) 'function) (next (addbinding (leftop expr)              ; func definition
                                                         (makeclosure (restof expr) state)
                                                         state)))
      ((eq? (operator expr) 'funcall)  (begin (funcall (lookup (leftop expr) state) ; function call
                                                       (param expr)
                                                       state
                                                       throw)
                                              (next state)))
      (else (next state)))))

; M_state function that deals with statement blocks
(define block
  (lambda (tree state return next continue break throw)
    (if (null? tree)
        (next state)
        (M_state-cpt (car tree)
                     state
                     return
                     (lambda (v) (block (cdr tree) v return next continue break throw))
                     continue
                     break
                     throw))))

; M_state function that deals with while loops
(define whileloop
  (lambda (condition body state return next continue break throw)
    (if (M_bool condition state throw)
        (M_state-cpt condition
                     state
                     return
                     (lambda (v1)
                       (M_state-cpt body
                                    v1
                                    return
                                    (lambda (v2)
                                      (whileloop condition body v2 return next continue break throw))
                                    (lambda (v2)
                                      (whileloop condition
                                                 body
                                                 (removelayer v2)
                                                 return
                                                 next
                                                 continue
                                                 break
                                                 throw))
                                    break
                                    throw))
                     continue
                     break
                     throw)
        (M_state-cpt condition state return next continue break throw))))

; M_state function that deals with try-catch-finally blocks
(define tcf
  (lambda (try catch finally state return next continue break throw)
    (if (null? finally)
        (block try ; no finally block
               state
               return
               next
               continue
               break
               (lambda (v e)
                 (block (rightop catch)
                        (addbinding (car (leftop catch)) e v)
                        return
                        next
                        continue
                        break
                        throw)))
        (block try ; yes finally block
               state
               (lambda (v)     ; newreturn
                 (block (leftop finally)
                        v
                        return
                        return
                        continue
                        break
                        throw))
               (lambda (v)     ; newnext
                 (block (leftop finally)
                        v
                        return
                        next
                        continue
                        break
                        throw)) 
               (lambda (v)     ; newcontinue
                 (block (leftop finally)
                        v
                        return
                        continue
                        continue
                        break
                        throw))
               (lambda (v)     ; newbreak
                 (block (leftop finally)
                        v
                        return
                        break
                        continue
                        break
                        throw))
               (lambda (v1 e1) ; newthrow
                 (block (rightop catch)
                        (addbinding (car (leftop catch)) e1 v1)
                        (lambda (v2)    ; newreturn
                          (block (leftop finally)
                                 v2
                                 return
                                 return
                                 continue
                                 break
                                 throw))
                        (lambda (v2)    ; newnext
                          (block (leftop finally)
                                 v2
                                 return
                                 next
                                 continue
                                 break
                                 throw))
                        (lambda (v2)    ; newcontinue
                          (block (leftop finally)
                                 v2
                                 return
                                 continue
                                 continue
                                 break
                                 throw))
                        (lambda (v2)    ; newbreak
                          (block (leftop finally)
                                 v2
                                 return
                                 break
                                 continue
                                 break
                                 throw))
                        (lambda (v2 e2) ;newthrow
                          (block (leftop finally)
                                 v2
                                 (lambda (v3) (throw v3 e2))
                                 (lambda (v3) (throw v3 e2))
                                 (lambda (v3) (throw v3 e2))
                                 (lambda (v3) (throw v3 e2))
                                 (lambda (v3) (throw v3 e2))))))))))
