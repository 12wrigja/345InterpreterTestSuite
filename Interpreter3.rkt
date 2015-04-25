;Adam DeFranco Team Members: James Wright and Joseph Satterfield
;Interpreter Project, Part 3
;April 8, 2015

(load "functionParser.scm")

;This is just me defining the null state. This is more for making testing easy than anything
(define nullState '((()())))

;Another test method to make sure the parser is working in the manner expected. 
(define returnParse
  (lambda (file)
    (parser file)))

;Checks if a binding already exists in the state, if it does not exist it will add the binding to the state. If the binding already exists, the state given is returned. 
(define addToLevel
  (lambda (state variable value)
    (cond
      ((null? (car state)) (list (list variable) (list (box value))))
      ((eq? (caar state) variable) state)
      (else ((lambda (resultstate) 
               (list (cons (caar state) (car resultstate)) (cons (caadr state) (cadr resultstate)))) 
               (addToLevel (list (cdar state) (cdadr state)) variable value))))))

;Will remove the binding from the state if it exists. If there is no binding it will return the given state. 
(define Remove
  (lambda (state variable)
    (cond
      ((null? (car state)) state)
      ((eq? (caar state) variable) (Remove (list (cdar state) (cdadr state)) variable))
      (else ((lambda (resultstate)
              (list (cons (caar state) (car resultstate)) (cons (caadr state) (cadr resultstate)))) 
              (Remove (list (cdar state) (cdadr state)) variable))))))

;Looks up if a variable exists accross a whole state. Returns the value assigned to it if so, if not throws an error. 
(define lookup
  (lambda (state variable)
    (cond
      ((null? state) (error "Variable" variable "was not in the given state."))
      ((eq? separator (car state)) (lookup (cdr state) variable))
      ((hasBeenDeclaredInLevel variable (car state)) (unbox (lookupInLevel variable (car state))))
      (else (lookup (cdr state) variable)))))

;Looks up a variable in a given level. Returns the value of the variable if it exists in the level, otherwise throws an error if no binding/assignment. 
(define lookupInLevel
  (lambda (variable level)
    (cond
      ((null? (car level)) (error "Variable" variable "was not in the given level."))
      ((eq? (caar level) variable) ((lambda (value) (cond ((null? value) (error variable "Variable has not been assigned.")) (else value))) (caadr level)))
      (else (lookupInLevel variable (list (cdar level) (cdadr level)))))))

;///////////////////////////////////////////DECLARE SECTION/////////////////////////////////////////////////////////////

;MStateDeclare will check if a variable was already declared. If it was, it will error, if not it will add it to the state table.
(define MStateDeclare
  (lambda (stmt state return)
    (cond
      ((hasBeenDeclaredInScope (declareVarName stmt) state) (error (declareVarName stmt) "variable is being re-defined"))
      (else (return (add state (declareVarName stmt) (declareValue stmt state)))))))

;returns the value a variable is being declared to or empty list if no value defined 
(define declareValue
  (lambda (stmt state)
    (cond
      ((null? (cddr stmt)) '())
      (else (MValue (caddr stmt) state (lambda (v1 v2) v1))))))

;returns the variable name from a given declare statement
(define declareVarName cadr)
   
;Helper for declare to check if a variable name has already been declared within a passed in level                                                  
(define hasBeenDeclaredInLevel 
  (lambda (varname level)
      (if (or (eq? level separator) (not (member varname (car level)))) #f #t))) 
      
;Helper for declare to return the variable name in a declare statement
(define declareGetVariable cadr)

;Helper for declare to return the value of the variable declared. If there was no assignment, it returns the null list
(define declareGetValue
  (lambda (l)
    (if (null? (cddr l))
        '()
        (caddr l))))

;/////////////////////////////////////////ASSIGN SECTION/////////////////////////////////////////////////////////////

;MState assign will add the new value to a variable if it has already been assigned. If it is not found in the state, remove will throw an error
(define MStateAssign
  (lambda (stmt state return)
    (cond
      ((not (hasBeenDeclaredInState (assignVarName stmt) state)) (error (assignVarName stmt) "has not yet been declared"))
      (else (return (replace state (assignVarName stmt) (assignValue stmt state)))))))

;assign-value retrieves the value that is being assigned to a variable in an assignment statement
(define assignValue
  (lambda (stmt state)
    (MValue (caddr stmt) state (lambda (v1 v2) v1))))

;gets the name of the variable being assigned in an assignment statement
(define assignVarName cadr)

; MValueArith evaluates arithmetic expressions
(define MValueArith
  (lambda (expression state return)
      (cond
        ((eq? '+ (operator expression)) (applyOperator + (leftoperand expression) (rightoperand expression) state return))
        ((eq? '- (operator expression)) (MValue (leftoperand expression) state (lambda (leftVal leftState)
                                                                                  (MValue (rightoperand expression) state (lambda (rightVal rightState)
                                                                                                                                (if (null? rightVal) (return (- 0 leftVal) state) (return (- leftVal rightVal) state)))))))
        ((eq? '/ (operator expression)) (applyOperator quotient (leftoperand expression) (rightoperand expression) state return))
        ((eq? '* (operator expression)) (applyOperator * (leftoperand expression) (rightoperand expression) state return))
        ((eq? '% (operator expression)) (applyOperator modulo (leftoperand expression) (rightoperand expression) state return))
        (else (error 'bad-operator)))))

;MStateBoolean will evaluate a boolean statement and return true or false
(define MValueBoolean
  (lambda (expression state return)
      (cond
        ((eq? expression 'true) (return #t state))
        ((eq? expression 'false) (return #f state))
        ((eq? '== (operator expression)) (applyOperator eq? (leftoperand expression) (rightoperand expression) state return))
        ((eq? '!= (operator expression)) (applyOperator (lambda (v1 v2) (not (eq? v1 v2))) (leftoperand expression) (rightoperand expression) state return))
        ((eq? '< (operator expression)) (applyOperator < (leftoperand expression) (rightoperand expression) state return)) 
        ((eq? '> (operator expression)) (applyOperator > (leftoperand expression) (rightoperand expression) state return))
        ((eq? '<= (operator expression)) (applyOperator <= (leftoperand expression) (rightoperand expression) state return))
        ((eq? '>= (operator expression)) (applyOperator >= (leftoperand expression) (rightoperand expression) state return))
        ((eq? '&& (operator expression)) (applyOperator (lambda (v1 v2) (and v1 v2)) (leftoperand expression) (rightoperand expression) state return))
        ((eq? '|| (operator expression)) (applyOperator (lambda (v1 v2) (or v1 v2)) (leftoperand expression) (rightoperand expression) state return))
        ((eq? '! (operator expression)) (MValue (leftoperand expression) state (lambda (leftVal stateLeft) (return (not leftVal) state))))
        (else (error 'bad-operator)))))

;The generic MValue method which will call MValueArith if it is an arithmetic expression or MValueBoolean if it is a boolean expression.
(define MValue
  (lambda (expression state return)
    (cond
      ((null? expression) (return '() state)) 
      ((number? expression) (return expression state))
      ((eq? 'true expression) (return #t state))
      ((eq? 'false expression) (return #f state))
      ((atom? expression) (return (lookup state expression) state)) 
      ((isFunction expression) (callFunction expression state return))
      ((member (operator expression) '(+ - / * %)) (MValueArith expression state return))
      ((member (operator expression) '(== != < > <= >= && || !)) (MValueBoolean expression state return)))))

;applies given operator to given operands
(define applyOperator
  (lambda (operator leftOperand rightOperand state return)
    (MValue leftOperand state (lambda (leftVal stateLeft)
                                    (MValue rightOperand state (lambda (rightVal stateRight)
                                                                     (return (operator leftVal rightVal) state)))))))

;MValueReturn will take a return statement and return the statement right of "return"
(define MValueReturn
  (lambda (stmt state return break continue)
    (MValue (cadr stmt) state return)))

;Helper to get the operator from an expression in prefix notation
(define operator car)
   
;Helper to get the left operand from an expression in prefix notation
(define leftoperand cadr) 

;Helper to get the right operand from an expression in prefix notation
(define rightoperand 
  (lambda (expression)
    (if (null? (cddr expression))
        '()
        (caddr expression))))

;Used to test for if something is an atom
(define (atom? x) (not (or (pair? x) (null? x))))

;//////////////////////////////////////IF STATEMENT SECTION///////////////////////////////////////

;MValueIf will check if the if condition is true, if so it'll evaluate the statement in the if, otherwise it'll evalueate the statement in the else
(define MValueIf
  (lambda (stmt state return break continue)
    (MValueBoolean (ifStmtCond stmt) state (lambda (v1 v2) 
                                                 (if v1 (valuesControl (list (ifStmtTrue stmt)) state return break continue) (valuesControl (list (ifStmtFalse stmt)) state return break continue))))))

;returns the condition of an if statement
(define ifStmtCond cadr)

;returns the statement to be evaluated if the condition is true
(define ifStmtTrue caddr)

;returns the satement to be evaluated if the condition is false
(define ifStmtFalse 
  (lambda (stmt)
    (if (null? (cdddr stmt)) '() (cadddr stmt))))

;Computes the state change that happens by evaluating an if statement and any substatements
(define MStateIf
  (lambda (stmt state return)
    (cond
      ((MValueBoolean (ifStmtCond stmt) state) (return (stateStatement (ifStmtTrue stmt) state)))
      (else (stateStatement (ifStmtFalse stmt) state return)))))

;Helper for return to get the expression returned 
(define returnGetExpression cadr)

;Helper for if statement to get the first condition
(define ifStmtGetCond cadr)

;Helper for if statment to get the "then" statement
(define ifStmtGetThenStmt caddr)

;Helper for if statement to get the optional else statement
(define ifStmtGetOptionalElse
  (lambda (l)
    (if (null? (cdddr l))
        '()
        (cadddr l))))

;Value will loop through the parsed code and send valueStatement each statement one by one. This is basically looping through the parsing tree and evaluating each statement along the way
(define value
  (lambda (stmts state return)
    (valuesControl stmts 
                    state 
                    return 
                    (lambda (returnValue breakState)
                      (error "Break statement outside of control structure"))
                    (lambda (returnValue continueState)
                      (error "Continue statement outside of control structure")))))

;valueStatement will take in a statement, determine what kind of statement it is, then run the Mvalue of the statement using the Mstate of the statement
(define valueStatement
  (lambda (stmt state return break continue)
    (cond
      ((null? stmt) (return '() state))
      ((isFunction stmt) (callFunction stmt state (lambda (returnValue returnState) (return '() returnState))))
      ((isWhile stmt) (MValueWhile stmt state return break continue))
      ((isReturn stmt) (MValueReturn stmt state return break continue))
      ((isIf stmt) (MValueIf stmt state return break continue))
      (else (stateStatement stmt state (lambda (v)
                                     (return '() v)))))))

(define isFunction 
  (lambda (stmt)
    (if (eq? (car stmt) 'funcall) #t #f)))

;Conputes the MValue for a while statement
(define MValueWhile
  (lambda (stmt state return break continue)
    (MValueBoolean (whileStmtCond stmt) state (lambda (boolVal ifstate)
                                                    (if boolVal (valuesControl (list (whileStmts stmt))
                                                                                state 
                                                                                (lambda (returnVal returnState)
                                                                                  (if (not (null? returnVal)) 
                                                                                      (return returnVal returnState) 
                                                                                      (MValueWhile stmt returnState return break continue)))
                                                                                (lambda (breakReturnVal breakState)
                                                                                  (return breakReturnVal breakState))
                                                                                (lambda (continueReturnVal continueState)
                                                                                  (MValueWhile stmt continueState return break continue)))
                                                        (return '() state))))))

(define whileStmtCond cadr)
(define whileStmts caddr)

;returns the new state from running a list of commands
(define valuesControl
  (lambda (stmts state return break continue)
    (cond
      ((or (null? stmts) (null? (car stmts))) (return '() state))
      ((isBlock (car stmts)) (valuesControl (cdar stmts) (addLayer state) (lambda (returnVal newState)
                                                                            (cond
                                                                              ((eq? returnVal #t) (return 'true newState))
                                                                              ((eq? returnVal #f) (return 'false newState))
                                                                              ((not (null? returnVal)) (return returnVal newState))
                                                                              (else (valuesControl (cdr stmts) (removeLayer newState) return break continue))))
                                      break
                                      continue))
      ((isBreak (car stmts)) (break '() (removeLayer state)))
      ((isContinue (car stmts)) (continue '() (removeLayer state)))
      (else (valueStatement (car stmts) state (lambda (returnVal newState)
                                            (cond
                                              ((eq? returnVal #t) (return 'true newState))
                                              ((eq? returnVal #f) (return 'false newState))
                                              ((not (null? returnVal)) (return returnVal newState))
                                              (else (valuesControl (cdr stmts) newState return break continue)))) break continue)))))

;returns whether given statement is an If statement or not
(define isIf
  (lambda (stmt)
    (eq? (car stmt) 'if)))

;returns whether given statement is a variable declaration statement or not
(define isVarDeclaration
  (lambda (stmt)
    (eq? (car stmt) 'var)))

;returns whether given statement is a return statement or not
(define isReturn
  (lambda (stmt)
    (eq? (car stmt) 'return)))

;returns whether given statement is an assignment statement or not
(define isAssign
  (lambda (stmt)
    (eq? (car stmt) '=)))

;retursn whether given statement is a while statement or not
(define isWhile
  (lambda (stmt)
    (eq? (car stmt) 'while)))

;returns whether given statement is a block statement or not
(define isBlock
  (lambda (stmt)
    (eq? (car stmt) 'begin)))

;returns whether given statement is a break statement or not
(define isBreak
  (lambda (stmt)
    (eq? (car stmt) 'break)))

;returns whether given statement is a continue statement
(define isContinue
  (lambda (stmt)
    (eq? (car stmt) 'continue)))

;stateState will check a statement to see which kind of statement it is and change the state accordingly
(define stateStatement
  (lambda (stmt state return)
    (cond
      ((null? stmt) (return state))
      ((isAssign stmt) (MStateAssign stmt state return))
      ((isVarDeclaration stmt) (MStateDeclare stmt state return ))
      ((isIf stmt) (MStateIf stmt state return))
      ((isWhile stmt) (MStateWhile stmt state return))
      (else (return state)))))

;Computes state changes that occur during a while statement
(define MStateWhile
  (lambda (stmt state return)
    (if (MValueBoolean (whileStmtCond stmt) state) (MStateWhile stmt (stateStatement (whileStmts stmt) state return) return) (return state))))
    
;The main method to run a parse tree through my code   
(define interpret
  (lambda (file)
    (callFunction '(function main ()) (setGlobalState (parser file) nullState (lambda (v1 v2) v2)) (lambda (v1 v2) v1))))

(define defaultReturn (lambda (returnVal state) returnVal))

;//////////////////////////////////////INTERPRETER PART 2 SECTION////////////////////////////////////////////

;Defines an empty layer
(define newStateLevel '(()()))
(define newState (list newStateLevel))

;Pushes a layer onto a state
(define addLayer
  (lambda (state)
    (cons newStateLevel state)))

;Pops a layer from a state
(define removeLayer
  (lambda (state)
    (cdr state)))

;Helper for declare to check if a variable name has already been declared within an entire state                                                  
(define hasBeenDeclaredInState 
  (lambda (varname state)
      (cond
        ((null? state) #false)
        ((eq? separator (car state)) (hasBeenDeclaredInState varname (cdr state)))
        ((hasBeenDeclaredInLevel varname (car state)) #t)
        (else (hasBeenDeclaredInState varname (cdr state))))))

;Adds a binding to the first level of the state if it does not exist anywhere else in the state
(define add
  (lambda (state variable value)
    (cond
      ((not (hasBeenDeclaredInScope variable state)) (cons (addToLevel (car state) variable value) (cdr state)))
      (else state))))

;Replaces a variable if it exists in any level of the state
(define replace
  (lambda (state variable newVal)
    (cond
      ((null? state) state)
      ((hasBeenDeclaredInLevel variable (car state)) (cons (replaceInLevel (car state) variable newVal) (cdr state)))
      (else (cons (car state) (replace (cdr state) variable newVal))))))

;Replaces a variables within a level
(define replaceInLevel
  (lambda (level variable newVal)
    (cond
      ((null? (car level)) level)
      ((eq? (caar level) variable) (begin (set-box! (caadr level) newVal) level))
      (else ((lambda (resultState)
               (list (cons (caar level) (car resultState)) (cons (caadr level) (cadr resultState)))) (replaceInLevel (list (cdar level) (cdadr level)) variable newVal))))))

;Remove removes a variable binding from the state if it exists. Returns the given state if there is no variable bound in this state
(define removeInLevel
  (lambda (level variable)
    (cond
      ((null? (car level)) level)
      ((eq? (caar level) variable) (removeInLevel (list (cdar level) (cdadr level)) variable))
      (else ((lambda (resultstate)
               (list (cons (caar level) (car resultstate)) (cons (caadr level) (cadr resultstate)))) (removeInLevel (list (cdar level) (cdadr level)) variable))))))

;Returns whether a variable has been assigned anywhere within the state
(define assigned
  (lambda (variable state)
    (cond
      ((null? state) #f)
      ((assignedInLevel variable (car state)) #t)
      (else (assigned variable (cdr state))))))

;Returns whether a variable has been assigned a value within a certain state
(define assignedInLevel
  (lambda (variable level)
    (cond
      ((null? (car level)) #f)
      ((eq? (unbox (caar level)) variable) (not (null? (caadr level))))
      (else (assignedInLevel variable (list (cdar level) (cdadr level)))))))

;////////////////////////////////INTERPRETER PART 3 SECTION//////////////////////////////////////

;returns whether or not the given statement is a function definition
(define isFunctionDeclaration
  (lambda (stmt)
    (eq? (car stmt) 'function)))

;Creates a closure using a function declaration statement
(define createClosure
  (lambda (stmt state)
    (list (caddr stmt) (cadddr stmt) (lambda (actualParam currentState) (preprocessInnerFunctions (cadddr stmt) (bindAllInState (caddr stmt) actualParam (addLayer (addSeparator (trimStates currentState state))) currentState) (lambda (v1 v2) v2))))))

;Binds actual parameters to formal parameters into the ouput state using the reference state to look up the values of the actual parameters 
(define bindAllInState 
  (lambda (formalParams actualParams outputState referenceState)
    (cond
      ((null? formalParams) outputState)
      ((not (eq? (length formalParams) (length actualParams))) (error "Mismatched function and actual parameters"))
      (else (bindAllInState (cdr formalParams) (cdr actualParams) (add outputState (car formalParams) (MValue (car actualParams) referenceState (lambda (v1 v2) v1))) referenceState)))))
    
;if variable declaration or function declaration, add it to the state, if not throw an error
(define setGlobalState
  (lambda (stmts state return)
    (cond
      ((null? stmts) (return '() state))
      ((isVarDeclaration (car stmts)) (value (list (car stmts)) state (lambda (returnVal returnState)
                                                           (setGlobalState (cdr stmts) returnState return))))
      ((isFunctionDeclaration (car stmts)) (setGlobalState (cdr stmts) (add state (cadar stmts) (createClosure (car stmts) state)) return))
      (else error "The statement" (caar stmts) "can only be a variable declaration or a function declaration."))))

;Trims off of current execution state to get the scope of the function 
(define trimStates 
  (lambda (state1 state2)
    (cond
      ((eq? (length state1) (length state2)) state1)
      (else (trimStates (cdr state1) state2)))))

;Used to separate the scope of different levels of functions
(define separator "|")

;Pushes a separator to the state 
(define addSeparator 
  (lambda (state)
    (cons separator state)))

;Checks if a variable has been declared within the scope
(define hasBeenDeclaredInScope
  (lambda (varname state)
      (cond
        ((null? state) #false)
        ((eq? separator (car state)) #false)
        ((hasBeenDeclaredInLevel varname (car state)) #t)
        (else (hasBeenDeclaredInScope varname (cdr state))))))

;Used for executing function calls 
(define callFunction
  (lambda (stmt state return)
    (value (cadr (lookup state (cadr stmt))) ((caddr (lookup state (cadr stmt))) (cddr stmt) state) (lambda (returnVal returnState) (return returnVal state)))))

;Check for function declarations within a function
(define preprocessInnerFunctions
  (lambda (stmts state return)
    (cond
      ((null? stmts) (return '() state))
      ((isFunctionDeclaration (car stmts)) (preprocessInnerFunctions (cdr stmts) (add state (cadar stmts) (createClosure (car stmts) state)) return))
      (else (preprocessInnerFunctions (cdr stmts) state return)))))