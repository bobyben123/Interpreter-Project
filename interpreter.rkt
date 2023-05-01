#lang racket
;; Audrey Zhu, Fletcher Li, Zongbo Zhang

(require "classParser.rkt")

;; INTERPRETER

; takes a file name and interprets the program written on that file
(define interpret
  (lambda (filename classname)
    (evaluate (parser filename) (createstate) classname)))

; searches the state for a class closure that matches the classname returns the closure if found else
; false
(define findClass
  (lambda (classname state)
    (getvar (lookup classname state))))

; finds and runs the main function of a given class
(define runmain
  (lambda (classname state return throw)
    (funcall (findmain (getfuncsfromclosure (findClass classname state)))
             '()
             state
             return
             throw)))

;helps find and return the closure of the main function of a class given its function list
(define findmain
  (lambda (lis)
    (getvar (lookup 'main lis))))
      

; evaluate a parse tree and return the final state
(define evaluate
  (lambda (tree state classname)
    (if (null? tree)
        (runmain (string->symbol classname)
                 state
                 (lambda (v) v)
                 (lambda (v e)
                   (if (number? e)
                       (error 'thrownerror (number->string e))
                       (error 'thrownerror e))))
        (evaluate (cdr tree)
                  (M_state (car tree)
                           state
                           (lambda (v) v)
                           (lambda (v e)
                             (if (number? e)
                                 (error 'thrownerror (number->string e))
                                 (error 'thrownerror e))))
                  classname))))

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
    (cons (cons (list name (box #t)) (toplayer state)) (restof state))))

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
      ((null? state)
       #f)
      ((eq? (firstname state) name)
       (firstval state))
      ((null? (toplayer state))
       (lookup name (restof state)))
      ((and (pair? name) (eq? (operator name) 'dot))
       (lookup (rightop name) (getfuncsfromclosure (getclosure name state))))
      (else
       (lookup name (cons (cdr (toplayer state)) (restof state)))))))

; given a dot operation and a state, find the class closure in the state
(define getclosure
  (lambda (expr state)
    (findClass (car (getvar (lookup (leftop expr) state))) state)))

; takes the output of lookup and returns the variable's value or the appropriate error
(define getvar
  (lambda (v)
    (cond
      ((eq? v #f) (error 'novar "Variable Not Declared"))
      ((eq? v #t) (error 'noval "Variable Not Initalized"))
      (else       (unbox v)))))

; adds an empty new layer to the top of the state
(define addlayer
  (lambda (state)
    (cons '() state)))

; removes the top layer of the state
(define removelayer restof)

;; FUNCTION-RELATED HELPER FUNCTIONS

; takes a function definition and a state and returns the function's closure
(define makefunclosure
  (lambda (func type)
    (list (getfuncname func) (getformparams func) (getfuncbody func) type)))

; takes an instance function definition and a state and returns the function's closure with 'this
; added to the parameters
(define instfunclosure
  (lambda (func type)
    (list (getfuncname func) (cons 'this (getformparams func)) (getfuncbody func) type)))

; takes an abstract function definition and a state and returns the function's closure with an empty
; body
(define abstfunclosure
  (lambda (func type)
    (list (getfuncname func) (getformparams func) '() type)))

; retrieves a function's name from its definition or its closure
(define getfuncname car)

; retrieves a function's formal parameters from its definition or its closure
(define getformparams cadr)

; retrieves a function's body from its definition or its closure
(define getfuncbody caddr)

; retrieves a function's compile time type from its closure
(define getcomptype cadddr)

; takes a function closure and a state and returns the layer of the state where the function resides
(define getfuncstate
  (lambda (closure state)
    (getfuncstate-cpt closure state (lambda (v) v))))

(define getfuncstate-cpt
  (lambda (closure state return)
    (cond
      ((null? state)                                 (error 'nofunc "Function not defined"))
      ((eq? (firstname state) (getcomptype closure)) (return state))
      ((null? (toplayer state))                      (getfuncstate-cpt closure
                                                                       (removelayer state)
                                                                       (lambda (v) v)))
      (else                                          (getfuncstate-cpt closure
                                                                       (cons (cdr (toplayer state))
                                                                             (restof state))
                                                                       (lambda (v)
                                                                         (return
                                                                          (cons (cons (caar state)
                                                                                      (toplayer v))
                                                                                (restof v)))))))))

; takes a set of actual parameters, formal parameters, function state, and current state, and binds
; the actual parameters to the current parameters
(define bindparams
  (lambda (aparams fparams fstate cstate return throw)
    (cond
      ((and (null? fparams) (null? aparams)) fstate)
      ((eq? (car fparams) 'this) (bindparams aparams (cdr fparams) fstate cstate return throw))
      ((or (null? fparams) (null? aparams))  (error 'mismatch "Number of arguments does not match"))
      (else                                  (bindparams (cdr aparams)
                                                         (cdr fparams)
                                                         (addbinding (car fparams)
                                                                     (M_val (car aparams)
                                                                            cstate
                                                                            return
                                                                            throw)
                                                                     fstate)
                                                         cstate
                                                         return
                                                         throw)))))

;; CLASS-RELATED HELPER FUNCTIONS

; takes a class definition and returns the class closure
(define makeclassclosure
  (lambda (expr state)
    (list (getclassname expr)
          (getsuper expr)
          (getfuncs (getclassbody expr) (getsuper expr) (getclassname expr) state)
          (getclassvars (getclassbody expr))
          (getinstvars (getclassbody expr))
          (getclassinstvals (getclassbody expr) (getsuper expr) state)
          (getconstr (getclassbody expr) (getclassname expr)))))

; takes a class definition/closure and returns the class name
(define getclassname car)

; takes a class definition/closure and returns the class superclass
(define getsuper cadr)

; takes a class definition/closure and returns the class body
(define getclassbody caddr)

; take the body of a class definition and the state and return a list of the closures of the class
; functions
(define getfuncs
  (lambda (tree super this state)
    (getfuncs-cpt tree super this state (lambda (v) v))))

(define getfuncs-cpt
  (lambda (tree super this state return)
    (cond
      ((and (null? tree) (null? super))
       (return '(())))
      ((null? tree)
       (return (getfuncsfromclosure (getvar (lookup super state)))))
      ((eq? (operator (car tree)) 'static-function)   ; static function
       (getfuncs-cpt (cdr tree)
                     super
                     this
                     state
                     (lambda (v)
                       (return (addbinding (getfuncname (restof (car tree)))
                                           (makefunclosure (restof (car tree)) this)
                                           v)))))
      ((eq? (operator (car tree)) 'function)          ; instance function
       (getfuncs-cpt (cdr tree)
                     super
                     this
                     state
                     (lambda (v)
                       (return (addbinding (getfuncname (restof (car tree)))
                                           (instfunclosure (restof (car tree)) this)
                                           v)))))
      ((eq? (operator (car tree)) 'abstract-function) ; abstract functions
       (getfuncs-cpt (cdr tree)
                     super
                     this
                     state
                     (lambda (v)
                       (return (addbinding (getfuncname (restof (car tree)))
                                           (abstfunclosure (restof (car tree)) this)
                                           v)))))
      (else
       (getfuncs-cpt (cdr tree) super this state return)))))

; takes a class closure and gets its function list
(define getfuncsfromclosure caddr)

; takes the body of a class definition and returns a list of the class variables
(define getclassvars
  (lambda (tree)
    (getclassvars-cpt tree (lambda (v) v))))

(define getclassvars-cpt
  (lambda (tree return)
    (cond
      ((null? tree)
       (return '()))
      ((eq? (operator (car tree)) 'static-var)
       (return (getclassvars-cpt (cdr tree)
                                 (lambda (v)
                                   (return (cons (list (leftop (car tree)) (rightop (car tree)))
                                                 v))))))
      (else
       (getclassvars-cpt (cdr tree) return)))))

; takes the body of a class definition and returns a list of the instance variable names
(define getinstvars
  (lambda (tree)
    (getinstvars-cpt tree (lambda (v) v))))

(define getinstvars-cpt
  (lambda (tree return)
    (cond
      ((null? tree)                     (return '()))
      ((eq? (operator (car tree)) 'var) (getinstvars-cpt (cdr tree)
                                                         (lambda (v)
                                                           (return (cons (leftop (car tree)) v)))))
      (else                             (getinstvars-cpt (cdr tree) return)))))

; takes the body of a class definition and returns a list of the instance variable values
(define getclassinstvals
  (lambda (tree super state)
    (getclassinstvals-cpt tree super state (lambda (v) v))))

(define getclassinstvals-cpt
  (lambda (tree super state return)
    (cond
      ((and (null? super) (null? tree)) (return '()))
      ((null? tree)                     (return (getvalsfromclos (getvar (lookup super state)))))
      ((eq? (operator (car tree)) 'var) (getclassinstvals-cpt (cdr tree)
                                                              super
                                                              state
                                                              (lambda (v)
                                                                (return (cons (rightop (car tree))
                                                                              v)))))
      (else                             (getclassinstvals-cpt (cdr tree) super state return)))))

; takes a class closure and returns the list of instance variable values
(define getvalsfromclos
  (lambda (closure)
    (car (cdr (cddddr closure)))))

; takes a class closure and returns the list of instance variables
(define getvarsfromclos
  (lambda (closure)
    (car (cddddr closure))))
  
; takes the body of a class definition and the state and returns the closure of the constructor
(define getconstr
  (lambda (tree this)
    (cond
      ((null? tree)                             '())
      ((eq? (operator (car tree)) 'constructor) (makefunclosure (car tree)) this)
      (else                                     (getconstr (cdr tree) this)))))

;; INSTANCE-RELATED HELPER FUNCTIONS

; takes a run time type from a constructor call (e.g. (new A)) and returns the instance closure
(define makeinstclosure
  (lambda (runtype)
    (list (getclassname runtype) (getobjinstvals runtype))))

; takes a run time type and returns the values of the instance variables
(define getobjinstvals
  (lambda (runtype)
    (reverse (getvalsfromclos runtype))))

;; MAPPINGS

; maps an expression to a numerical value
(define M_val
  (lambda (expr state return throw)
    (M_val-cpt expr state (lambda (v) v) return throw)))
    
(define M_val-cpt
  (lambda (expr state next return throw)
    (cond
      ((or (number? expr) (eq? expr 'true) (eq? expr 'false)) ; atomic value
       (next expr))
      ((symbol? expr) (next (getvar (lookup expr state))))  ; variable
      ((eq? (operator expr) '+)                               ; addition
       (M_val-cpt (leftop expr)
                  state
                  (lambda (v1) (M_val-cpt (rightop expr)
                                          state
                                          (lambda (v2) (next (+ v1 v2)))
                                          return
                                          throw))
                  return
                  throw))
      ((and (eq? (operator expr) '-) (null? (cddr expr)))     ; opposite
       (M_val-cpt (leftop expr) state (lambda (v) (next (* -1 v))) return throw))
      ((eq? (operator expr) '-)                               ; subtraction
       (M_val-cpt (leftop expr)
                  state
                  (lambda (v1) (M_val-cpt (rightop expr)
                                          state
                                          (lambda (v2) (next (- v1 v2)))
                                          return
                                          throw))
                  return
                  throw))
      ((eq? (operator expr) '*)                               ; multiplication
       (M_val-cpt (leftop expr)
                  state
                  (lambda (v1) (M_val-cpt (rightop expr)
                                          state
                                          (lambda (v2) (next (* v1 v2)))
                                          return
                                          throw))
                  return
                  throw))
      ((eq? (operator expr) '/)                               ; division
       (M_val-cpt (leftop expr)
                  state
                  (lambda (v1)
                    (M_val-cpt (rightop expr)
                               state
                               (lambda (v2) (next (quotient v1 v2)))
                               return
                               throw))
                  return
                  throw))
      ((eq? (operator expr) '%)                               ; modulo
       (M_val-cpt (leftop expr)
                  state
                  (lambda (v1)
                    (M_val-cpt (rightop expr)
                               state
                               (lambda (v2) (next (remainder v1 v2)))
                               return
                               throw))
                  return
                  throw))
      ((eq? 'funcall (operator expr))
       (next (funcall (getvar (lookup (leftop expr) state)) (param expr) state return throw)))
      ((eq? 'new (operator expr))                             ; constructor call
       (next (makeinstclosure (getvar (lookup (leftop expr) state)))))
      ((eq? #t (M_bool expr state return throw))              ; true
       (next 'true))
      ((eq? #f (M_bool expr state return throw))              ; false
       (next 'false))
      ((eq? (operator expr) 'dot)                             ; dot operator
       (next (getdot expr state)))
      (else                                                   ; error
       (error 'unknownop "Bad Operator")))))

; Helps get the value of a variable for a dot operator
; Takes the instance closure name, variable, and state and returns the value of the variable
(define getdot
  (lambda (expr state)
    (list-ref (reverse (cadr (getvar (lookup (leftop expr) state))))
              (index-of (getvarsfromclos (getclosure expr state)) (rightop expr)))))

; M_val function for processing function calls
; takes a function closure and a list of actual parameters and returns the function's return value
(define funcall
  (lambda (closure params state return throw)
    (M_state (cons 'begin (getfuncbody closure))
             (bindparams params
                         (getformparams closure)
                         (addlayer (getfuncstate closure state))
                         state
                         return
                         throw)
             (lambda (v) v)
             throw)))

; maps an expression to a boolean value
(define M_bool
  (lambda (expr state return throw)
    (M_bool-cpt expr state (lambda (v) v) return throw)))

(define M_bool-cpt
  (lambda (expr state next return throw)
    (cond
      ((or (eq? expr #t) (eq? expr 'true))  (next #t))                                        ; #t
      ((or (eq? expr #f) (eq? expr 'false)) (next #f))                                        ; #f
      ((symbol? expr)                       (next (getvar (lookup expr state))))              ; var
      ((eq? (operator expr) '<)             (next (< (M_val (leftop expr) state return throw) ; <
                                                     (M_val (rightop expr) state return throw))))
      ((eq? (operator expr) '>)             (next (> (M_val (leftop expr) state return throw) ; >
                                                     (M_val (rightop expr) state return throw))))
      ((eq? (operator expr) '==)            (next (eq? (M_val (leftop expr) state return throw) ; =
                                                       (M_val (rightop expr) state return throw))))
      ((eq? (operator expr) '<=)            (next (<= (M_val (leftop expr) state return throw) ; <=
                                                      (M_val (rightop expr) state return throw))))
      ((eq? (operator expr) '>=)            (next (>= (M_val (leftop expr) state return throw) ; >=
                                                      (M_val (rightop expr) state return throw))))
      ((eq? (operator expr) '!=)            (next (not (eq? (M_val (leftop expr) state return throw)
                                                            (M_val (rightop expr)
                                                                   state
                                                                   return
                                                                   throw)))))
      ((eq? (operator expr) '!)             (M_bool-cpt (leftop expr)                           ; !
                                                        state
                                                        (lambda (v)
                                                          (next (not (M_bool-cpt v
                                                                                 state
                                                                                 next
                                                                                 return
                                                                                 throw))))
                                                        return
                                                        throw))
      ((eq? (operator expr) '&&)            (M_bool-cpt (leftop expr)                           ; and
                                                        state
                                                        (lambda (v1)
                                                          (M_bool-cpt (rightop expr)
                                                                      state
                                                                      (lambda (v2)
                                                                        (next
                                                                         (and (M_bool-cpt v1
                                                                                          state
                                                                                          next
                                                                                          return
                                                                                          throw)
                                                                              (M_bool-cpt v2
                                                                                          state
                                                                                          next
                                                                                          return
                                                                                          throw))))
                                                                      return
                                                                      throw))
                                                        return
                                                        throw))
      ((eq? (operator expr) '||)            (M_bool-cpt (leftop expr)                           ; or
                                                        state
                                                        (lambda (v1)
                                                          (M_bool-cpt (rightop expr)
                                                                      state
                                                                      (lambda (v2)
                                                                        (next
                                                                         (or (M_bool-cpt v1
                                                                                         state
                                                                                         next
                                                                                         return
                                                                                         throw)
                                                                             (M_bool-cpt v2
                                                                                         state
                                                                                         next
                                                                                         return
                                                                                         throw))))
                                                                      return
                                                                      throw))
                                                        return
                                                        throw))
      ((eq? 'funcall (operator expr))       (next (funcall (getvar (lookup (leftop expr) state))
                                                           (param expr)
                                                           state
                                                           return
                                                           throw)))
      (else                                 (next -1)))))

;; M_STATE FUNCTIONS

; takes an expression and a state and returns a new state
(define M_state
  (lambda (expr state return throw)
    (M_state-cpt expr
                 state
                 (lambda (v) v)
                 (lambda (v) v)
                 (lambda (v) (error 'badcontinue "Invalid Continue"))
                 (lambda (v) (error 'badbreak "Invalid Break"))
                 throw)))

(define M_state-cpt
  (lambda (expr state return next continue break throw)
    (cond
      ((null? expr)       (next state))
      ((not (pair? expr)) (next state))                                            ; not a statement
      ((and (eq? (operator expr) 'var) (null? (cddr expr)))                        ; declaration
       (next (addname (leftop expr) state)))
      ((eq? (operator expr) 'var)                                                  ; var x = y
       (next (addbinding (leftop expr) (M_val (rightop expr) state  return throw) state)))
      ((and (eq? (operator expr) '=) (eq? #f (lookup (leftop expr) state)))        ; no variable
       (error 'novar "Variable Not Declared"))                          
      ((eq? (operator expr) '=) (next (assign (leftop expr)                        ; assignment
                                              (M_val (rightop expr) state return throw)
                                              state)))
      ((and (or (eq? (operator expr) 'if) (eq? (operator expr) 'while))            ; error: condition
            (number? (M_bool (leftop expr) state return throw)))
       (error 'badcon "Bad Condition"))
      ((and (eq? (operator expr) 'if) (M_bool (leftop expr) state return throw))   ; if: then stmt
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
      ((eq? (operator expr) 'try)      (tcf (leftop expr)                         ; try-catch-finally
                                            (rightop expr)
                                            (cadddr expr)
                                            state
                                            return
                                            next
                                            continue
                                            break
                                            throw))
      ((eq? (operator expr) 'return)   (return (M_val (leftop expr) state return throw))) ; return
      ((eq? (operator expr) 'continue) (continue state))                            ; continue
      ((eq? (operator expr) 'break)    (break state))                               ; break
      ((eq? (operator expr) 'throw)    (throw state (M_val (leftop expr) state return throw)));throw
      ;((eq? (operator expr) 'function) (next (addbinding (leftop expr)              ; func def
      ;                                                   (makefunclosure (restof expr))
      ;                                                   state)))
      ((eq? (operator expr) 'funcall)  (next (begin (funcall (getvar (lookup (leftop expr) state))
                                                             (param expr)
                                                             state
                                                             return
                                                             throw)
                                                    state)))
      ((eq? (operator expr ) 'class)   (next (addbinding (leftop expr)               ; class def
                                                         (makeclassclosure (restof expr) state)
                                                         state)))
      ;((eq? (operator expr ) 'dot)     (next (handledot (lookup (leftop expr) state)
      ;                                                  (rightop expr))))
      (else                            (next state)))))

; Helps the dot operator figure out what the right operator is given the instance and state
; If it's a method than funcall is used, else the value is retrieved
;(define handledot
;  (lambda (instance expr)
;    (if (lookup expr instance) (funcall (lookup expr instance))
;        (error 'thrownerror "could not find expression"))))


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
    (if (M_bool condition state return throw)
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
               (lambda (v e) ; new throw
                 (block (rightop catch)
                        (addbinding (car (leftop catch)) e state)
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
