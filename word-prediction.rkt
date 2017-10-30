#lang racket
(require racket/gui)

; Lista com os elementos iniciais
(define lista
  '())

; Função que gera as previsões de palavras
#|
dot-flag -> #t se ultima palavra contem ".", #f ao contraio.
case-flag -> #t se a palavra atual está em minusculo, #f se não.
first-flag -> #t se o primeiro elemento a ser escrito para uma proxima plavra é maiusculo, #f se não.
all-flag -> #t se a palavra atual inteira for maiucula, #f se não.
|#
(define (gerar-resposta input last db)
  (let* ([dot-flag (string-suffix? last ".")]     
         [case-flag (string=? (string-downcase input) input)]
         [first-flag (if (not (equal? "" input))
                         (string=? (string-upcase (substring input 0 1)) (substring input 0 1))
                         #f)]
         [all-flag (and (string=? (string-upcase input) input) (> (string-length input) 1))]
         [f-filtro (λ (x) (string-prefix? (car x) (string-downcase input)))]
         [l-filtrada (filter f-filtro db)]
         [f-ordena-peso (λ (word1 word2) (> (cadr word1) (cadr word2)))]
         [l-ordenada-peso (sort l-filtrada f-ordena-peso)]
         [f-ordena-tam(λ (word1 word2) (< (string-length (car word1)) (string-length (car word2))))]
         [l-ordenada-tam (cond
                           [(and case-flag (not first-flag) (not dot-flag)) (sort l-ordenada-peso f-ordena-tam)]
                           [all-flag (map (λ (bloco) (cons (string-upcase (car bloco)) (cdr bloco))) (sort l-ordenada-peso f-ordena-tam))]
                           [dot-flag (map (λ (bloco) (cons (string-titlecase (car bloco)) (cdr bloco))) (sort l-ordenada-peso f-ordena-tam))]
                           [(and first-flag (not all-flag)) (map (λ (bloco) (cons (string-titlecase (car bloco)) (cdr bloco))) (sort l-ordenada-peso f-ordena-tam))])]
         [l-ordenada l-ordenada-tam])
    (cond
      [(>= (length l-ordenada) 3) (take l-ordenada 3)]
      [(= (length l-ordenada) 2) (append l-ordenada '(("")))]
      [(= (length l-ordenada) 1) (list (car l-ordenada) '("") '(""))]
      [else '(("") ("") (""))])))

; Função que atualiza o peso das palavras conforme vao sendo usadas
(define (atualiza-peso l)
  (set! test-db (map (λ (x) (if (equal? (car x) (car l))
                              (cons (car x) (cons (add1 (cadr x)) '()))
                              x)) test-db)))

; Abertura inicial do banco
(define test-db
  (map (λ (x) (cons x (cons 1 '()))) (string-split (file->string "dicionario.txt") "\n")))

; Tela principal
(define tela
  (new frame%
       [label "Previsão de Palavras"]
       [width 200]
       [height 300]
       [border 10]
       [spacing 10]))

; Caixa de texto aonde as palavras serão digitadas
(define caixa-texto
  (new text-field%
       [label #f]
       [parent tela]
       [style '(multiple)]
       [callback (λ (a e) (let* ([texto (string-split (send caixa-texto get-value) " ")]
                                 [last-word (if (> (length texto) 1) (cadr (reverse texto)) "")]
                                 [texto (if (null? texto)
                                            ""
                                            (last texto))])
                            (set! lista (gerar-resposta texto last-word test-db))
                            ;(print lista)
                            ;(print last-word)
                            (update-btn lista buttons)))]))

; Lista de botões
(define buttons
  (list
   (new button%
        [label ""]
        [parent tela]
        [style '(border)]
        [stretchable-width #t]
        [stretchable-height #t]
        [callback (λ (a e) (insert-word 1 lista))])
   (new button%
        [label ""]
        [parent tela]
        [stretchable-width #t]
        [stretchable-height #t]
        [callback (λ (a e) (insert-word 2 lista))])
   (new button%
        [label ""]
        [parent tela]
        [stretchable-width #t]
        [stretchable-height #t]
        [callback (λ (a e) (insert-word 3 lista))])))

; Função para inserir as palavras previstas corretas (clicadas) na caixa de texto
(define (insert-word f lista)
  (if (null? (reverse (cdr (reverse (string-split (send caixa-texto get-value) " ")))))
      (cond
        [(= f 1) (send caixa-texto set-value (string-join (cons (car (first lista)) '()))) (atualiza-peso (first lista))]
        [(= f 2) (send caixa-texto set-value (string-join (cons (car (second lista)) '()))) (atualiza-peso (second lista))]
        [(= f 3) (send caixa-texto set-value (string-join (cons (car (third lista)) '()))) (atualiza-peso (third lista))])
      (let ([lista-palavras (string-split (send caixa-texto get-value) " ")])
        (cond
          [(= f 1) (send caixa-texto set-value (string-join (append (take lista-palavras (- (length lista-palavras) 1)) (list (car (first lista)))))) (atualiza-peso (first lista))]
          [(= f 2) (send caixa-texto set-value (string-join (append (take lista-palavras (- (length lista-palavras) 1)) (list (car (second lista)))))) (atualiza-peso (second lista))]
          [(= f 3) (send caixa-texto set-value (string-join (append (take lista-palavras (- (length lista-palavras) 1)) (list (car (third lista)))))) (atualiza-peso (third lista))]))))

; Update na label dos botoes a cada nova previsão
(define (update-btn l btns)
  (cond
    [(null? l) '()]
    [else (send (car btns) set-label (caar l)) (update-btn (cdr l) (cdr btns))]))

(send tela show #t)
