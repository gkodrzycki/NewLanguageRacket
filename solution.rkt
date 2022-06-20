#lang plait
;Na podstawie kodu z listy 12 zad 6

;; abstract syntax -------------------------------

(define-type Op
  (add) (sub) (mul) (leq))

(define-type Exp
  (defE [d : (Listof Exp)] [for : Exp])
  (funE [s : Symbol] [arg : (Listof Symbol)] [e : Exp])
  (numE [n : Number])
  (varE [x : Symbol])
  (opE  [op : Op] [e1 : Exp] [e2 : Exp])
  (ifE  [b : Exp] [l : Exp] [r : Exp])
  (letE [x : Symbol] [e1 : Exp] [e2 : Exp])
  (appE [e1 : Symbol] [e2 : (Listof Exp)]))

;; parse ----------------------------------------

(define (parse-start [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `{define {ANY ...} for ANY} s)
     (defE (parse-fun (s-exp->list (second (s-exp->list s))))
           (parse (fourth (s-exp->list s))))]
    [else (error 'parse-start "Blad skladni")]))

(define (parse-fun [s : (Listof S-Exp)]) : (Listof Exp)
    (type-case (Listof S-Exp) s
      [(cons x xs)
       (cond 
         [(s-exp-match? `{fun SYMBOL {ANY ...} = ANY} x)
          (cons (funE (s-exp->symbol (second (s-exp->list x)))
                      (parse-arg (s-exp->list (third (s-exp->list x))))
                      (parse (list-ref (s-exp->list x) 4)))
          (parse-fun xs))]
         [else (error 'parse-fun "Bledy typ funkcji")])]
      [empty empty]))

(define (parse-arg [s : (Listof S-Exp)]) : (Listof Symbol)
  (type-case (Listof S-Exp) s
    [(cons x xs)
     (cond
       [(s-exp-match? `SYMBOL x) (cons (s-exp->symbol x) (parse-arg xs))]
       [else (error 'parse-arg "Nie jest symbolem")])]
    [empty empty]))
     
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s)
     (varE (s-exp->symbol s))]
    [(s-exp-match? `{ANY SYMBOL ANY} s)
     (opE (parse-op (s-exp->symbol (second (s-exp->list s))))
                 (parse (first (s-exp->list s)))
                 (parse (third (s-exp->list s))))]
    [(s-exp-match? `{ifz ANY then ANY else ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (fourth (s-exp->list s)))
          (parse (list-ref (s-exp->list s) 5)))]
    [(s-exp-match? `{let SYMBOL be ANY in ANY} s)
     (letE (s-exp->symbol (second (s-exp->list s)))
           (parse (fourth (s-exp->list s)))
           (parse (list-ref (s-exp->list s) 5)))]
    [(s-exp-match? `{SYMBOL {ANY ...}} s)
     (appE (s-exp->symbol (first (s-exp->list s))) (arg-to-list (s-exp->list (second (s-exp->list s)))))]
    [else (error 'parse "invalid input")]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '<=) (leq)]
    [else (error 'parse "unknown operator")]))

(define (arg-to-list [s : (Listof S-Exp)]) : (Listof Exp)
  (type-case (Listof S-Exp) s
    [(cons x xs) (cons (parse x) (arg-to-list xs))]
    [empty empty]))

;; (eval/apply) --------------------------------------

(define-type Value
  (numV [n : Number])
  (funV [x : Symbol] [args : (Listof Symbol)] [e : Exp] [env : Env]))

(define (eval [e : Exp] [env : Env]) : Value 
  (type-case Exp e
    [(defE d for)
     (let* ([new-env (extend-env-app d env)] ;letrec
            [v1 (eval-d d new-env)])
       (begin (Update new-env v1) 
              (eval for new-env)))]
    [(funE s arg pk) (funV s arg pk env)]
    [(numE n) (numV n)]
    [(varE x) (lookup-env x env)]
    [(opE op e1 e2) (numV (calc op (numV-n (eval e1 env)) (numV-n (eval e2 env))))]
    [(ifE b l r) (if (= (numV-n (eval b env)) 0)
                     (eval l env)
                     (eval r env))]
    [(letE x e1 e2) (eval e2 (extend-env env x (eval e1 env)))]
    [(appE e1 e2)   (apply (lookup-env e1 env) (eval-arg e2 env))]))

(define (calc [op : Op] [e1 : Number] [e2 : Number]) : Number
  (type-case Op op
    [(add) (+ e1 e2)]
    [(sub) (- e1 e2)]
    [(mul) (* e1 e2)]
    [(leq) (if (<= e1 e2)
               0              ;Prawda
               694202137)]))  ;Losowa liczba pierwsza

(define (eval-arg [s : (Listof Exp)] [env : Env])
  (type-case (Listof Exp) s
    [(cons x xs) (cons (eval x env) (eval-arg xs env))]
    [empty empty]))

(define (apply [v1 : Value] [v2 : (Listof Value)])
  (type-case Value v1
    [(funV s args e env) (eval-fun s args e v2 env)]
    [else (error 'apply "not a function")]))

(define (eval-fun [s : Symbol] [args : (Listof Symbol)] [e : Exp] [v2 : (Listof Value)] [env : Env])
  (type-case (Listof Value) v2
    [(cons x xs) (eval-fun s (rest args) e xs (extend-env env (first args) x))]
    [empty (eval e env)]))

(define (eval-d [d : (Listof Exp)] [env : Env])
  (type-case (Listof Exp) d
    [(cons x xs) (cons (eval x env) (eval-d xs env))]
    [empty empty]))

(define (Update [env : Env] [app : (Listof Value)])
  (type-case (Listof Value) app
    [(cons x xs)
     (begin
       (update-env! env (funV-x x) x)
       (Update env xs))]
    [empty void]))

(define (extend-env-app [app : (Listof Exp)] [env : Env])
  (type-case (Listof Exp) app
    [(cons x xs) (extend-env-undef (extend-env-app xs env) (funE-s x))]
    [empty env]))

(define (run [e : S-Exp]) : Number
  (numV-n (eval (parse-start e) empty)))

;; environments
(define-type Storable
  (valS [v : Value])
  (undefS))

(define-type Binding
  (bind [name : Symbol]
        [ref : (Boxof Storable)]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)

(define (extend-env-undef [env : Env] [x : Symbol]) : Env
  (cons (bind x (box (undefS))) env))

(define (extend-env [env : Env] [x : Symbol] [v : Value]) : Env
  (cons (bind x (box (valS v))) env))

(define (find-var [env : Env] [x : Symbol]) : (Boxof Storable)
  (type-case (Listof Binding) env
    [empty (error 'lookup "unbound variable")]
    [(cons b rst-env) (cond
                        [(eq? x (bind-name b))
                         (bind-ref b)]
                        [else
                         (find-var rst-env x)])]))
  
(define (lookup-env [x : Symbol] [env : Env]) : Value
  (type-case Storable (unbox (find-var env x))
    [(valS v) v]
    [(undefS) (error 'lookup-env "undefined variable")]))
   
(define (update-env! [env : Env] [x : Symbol] [v : Value]) : Void
  (set-box! (find-var env x) (valS v)))