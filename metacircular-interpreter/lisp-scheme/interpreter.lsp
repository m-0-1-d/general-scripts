#|
+=============+
| EXPRESSIONS |
+=============+
|#

...

#|
+======================+
| ADDITIONAL FUNCTIONS |
+======================+
|#

; вычисляет список операндов
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; сначала функция вычисляет предикат, потом вычисляет
; выполнить ли первое выражение или альтернативу
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; Она принимает в виде аргументов последовательность выражений 
; и окружение, и выполняет выражения в том порядке, в котором 
; они ей даны. Возвращаемое значение совпадает со значением 
; последнего выражения.
(define (eval-sequence exps env)
	(cond ((last-exp? exps) (eval (first-exp exps) env))
		  (else (eval (first-exp exps) env) 
				(eval-sequence (rest-exps exps) env))))

; собвенно, присваивание и переопределение переменной.
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

#|
+================+
| MAIN FUNCTIONS |
+================+
|#

; apply применяет процедуру к вычисленным аргументам
(define (apply procedure arguments)
  ; примитивные процедуры - просто вызывать apply-primitive-procedure
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        
        ; вызывает eval-sequence; составные процедуры она применяет, 
        ; по очереди вычисляя выражения, составляющие тело процедуры.
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        
        (else (error "Unknown type of procedures -- APPLY" procedure))))

; eval вычиляет параметры и процедуру
(define (eval exp env)
  ; self-evaluating - обычные числа, здесь вы возвращаем сами себя
  (cond ((self-evaluating? exp) exp)
        
        ; variable - нужно посмотреть окружение, чтобы найти значение перменной
        ((variable? exp) (lookup-variable-value exp env))
        
        ; строки - видимо, должно удалять "" и возвращать значение строки
        ((quoted? exp) (text-of-quotation exp))
        
        ; переопределение и присваивание переменной вызывает eval рекурсивно
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        
        ; если значение истино, выполнять условие; иначе выполнять альтернативу
        ((if? exp) (eval-if exp env))
        
        ; нужно приготовить лямбду к использованию: упаковать ее вместе в аргументами, телом и окружением
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        
        ; требует выполнение своих подвыражений в порядке появления
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        
        ; преобразуются во вложенные значения if, а потом вычисляются
        ((cond? exp) (eval (cond->if exp) env))
        
        ; Когда мы вычислили операции и операнды - мы передаем их в apply
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        
        (else (error "Unknown type of expression -- EVAL" exp))))


