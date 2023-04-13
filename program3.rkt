#lang racket
(require data/maybe)
(require data/monad)
;;;;;;;;Data Structures;;;;;;;;;;;;
;;;;;;;;Stack Operatsions;;;;;;;;;;

(define (maybe-fail err)
  (list 'error err))

;; Create an empty stack
(define empty-stack '())

;; Push item onto stack
(define (push item stack)
  (cons item stack))

;; Pop item off stack
(define (pop stack)
  (if (null? stack)
      (maybe-fail "Error: Stack is empty")
      (just (values (cdr stack) (car stack)))))

;; Peek at the top item on the stack
(define (peek stack)
  (if (null? stack)
      (maybe-fail "Error: Stack is empty")
      (just (car stack))))

(define (stack-empty? stack)
  (null? stack))

(define (add x y)
  (+ x y))

(define (sub x y)
  (- x y))

(define (mul x y)
  (* x y))

(define (div x y)
  (if (= y 0)
      (maybe-fail "Division by zero")
      (just (/ x y))))

(define (clear-stack stack)
  (just empty-stack))

(define (show-stack stack)
  (display stack)
  (newline)
  (just stack))

;; Display the top item on the stack
(define (top-stack stack)
  (if (stack-empty? stack)
      (maybe-fail "Error: Stack is empty")
      (begin
        (display (peek stack))
        (newline)
        (just stack))))

(define (size-stack stack)
  (display (length stack))
  (newline)
  (just stack))

(define (duplicate-top stack)
  (if (stack-empty? stack)
      (maybe-fail "Error: Stack is empty")
      (push (from-just (peek stack)) stack)))

(define (apply-operation op stack)
  (case op
    ((ADD SUB MUL DIV)
     (if (< (length stack) 2)
         (maybe-fail "Too few items on stack")
         (let* ([stack-after-pop1 (from-just (pop stack))]
                [item1 (car stack-after-pop1)]
                [stack-after-pop2 (from-just (pop stack-after-pop1))]
                [item2 (car stack-after-pop2)])
           (case op
             ((ADD) (just (add item2 item1) stack-after-pop2))
             ((SUB) (just (sub item2 item1) stack-after-pop2))
             ((MUL) (just (mul item2 item1) stack-after-pop2))
             ((DIV) (div item2 item1))))))  
    ((CLR) (just empty-stack))
    ((SHOW) (just (show-stack stack)))
    ((TOP) (just (top-stack stack)))
    ((SIZ) (just (size-stack stack)))
    ((DUP) (just (duplicate-top stack)))
    ((END) (just empty-stack))
    (else (maybe-fail (format "Unknown operation: ~a" op)))))

(define (process-command input stack)
  (apply-operation (string->symbol (string-upcase input)) stack))


(define (main-loop stack)
  (display "> ")
  (let* ([input (read-line)]
         [command (string-upcase input)]
         [result (process-command command stack)])
    (cond
      [(string=? command "END") (void)]
      [(maybe? result)
       (begin
         (display "Error: ")
         (display (from-just result))
         (display ", Stack: ")
         (display stack)
         (newline))]
      [else
       (main-loop (from-just result))])))

(define (run-program)
  (main-loop empty-stack))

(run-program)
