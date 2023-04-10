#lang racket


(define empty-stack '())

(define (push item stack)
  (cons item stack))

(define (pop stack)
  (if (null? stack)
      (values '() #f) ; Return an empty stack and a #f value to indicate that the stack was empty
      (values (cdr stack) (car stack)))) ; Return the updated stack and the popped item

(define (peek stack)
  (if (null? stack)
      #f ; Return #f if the stack is empty
      (car stack))) ; Return the top item on the stack

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
      #f ; Return #f to indicate division by zero error
      (/ x y)))

(define (clear-stack stack)
  empty-stack)

(define (show-stack stack)
  (display stack)
  (newline)
  stack)

(define (top-stack stack)
  (if (stack-empty? stack)
      (begin
        (display "Error: Stack is empty")
        (newline))
      (begin
        (display (peek stack))
        (newline)))
  stack)

(define (size-stack stack)
  (display (length stack))
  (newline)
  stack)

(define (duplicate-top stack)
  (if (stack-empty? stack)
      (begin
        (display "Error: Stack is empty")
        (newline)
        stack)
      (push (peek stack) stack)))

(define (apply-operation op stack)
  (maybe
   (lambda (err)
     (begin
       (display "Error: ")
       (display err)
       (newline)
       stack))
   (case op
     ((ADD)
      (if (< (length stack) 2)
          (maybe-fail "Too few items on stack")
          (let* ((stack-after-pop1 item1) (pop stack))
                 (stack-after-pop2 item2) (pop stack-after-pop1))
            (push (add item2 item1) stack-after-pop2))))
     ((SUB)
      (if (< (length stack) 2)
          (maybe-fail "Too few items on stack")
          (let* ((stack-after-pop1 item1) (pop stack))
                 (stack-after-pop2 item2) (pop stack-after-pop1))
            (push (sub item2 item1) stack-after-pop2))))
     ((MUL)
      (if (< (length stack) 2)
          (maybe-fail "Too few items on stack")
          (let* ((stack-after-pop1 item1) (pop stack))
                 (stack-after-pop2 item2) (pop stack-after-pop1))
            (push (mul item2 item1) stack-after-pop2))))
     ((DIV)
      (if (< (length stack) 2)
          (maybe-fail "Too few items on stack")
          (let* ((stack-after-pop1 item1) (pop stack))
                 (stack-after-pop2 item2) (pop stack-after-pop1))
            (if (zero? item1)
                (maybe-fail "Division by zero")
                (push (div item2 item1) stack-after-pop2))))))
     ((CLR) (clear-stack stack))
     ((SHOW) (show-stack stack))
     ((TOP) (top-stack stack))
     ((SIZ) (size-stack stack))
     ((DUP) (duplicate-top stack))
     ((END) (clear-stack stack))
     (else (maybe-fail (format "Unknown operation: ~a" op))))))


     (define (get-command-from-user)
  (let ((input (read-line)))
    (string->symbol (string-upcase input))))
