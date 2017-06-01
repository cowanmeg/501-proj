#lang rosette/safe

;; Hard coded identity matrices to start circuit with
(define identity2 (bv 33825 16))
(define identity3 (bv 9241421688590303745 36))


;;;; Manipulation functions ;;;;;
(define (extract-row r d circ)
  ;;(assert (and (>= r 0) (< r d)))
  (printf "Extract-row: r=~a d=~a\n" r d)
  (define end (- (* d (+ r 1)) 1))
  (define start (+ 1 (- end d)))
  (assert (= d (+ 1 (- end start))))
  (extract end start circ)
  )

(define (move r d row)
  ;;(assert (and (>= r 0) (< r d)))
  (printf "Move: r=~a d=~a\n" r d)
  (define y (zero-extend row (bitvector (* d d))))
  (displayln y)
  (define x (bvshl
   y
   ;(bv (* dest d) (* d d)))
   (integer->bitvector (* r d) (bitvector (* d d)))))
  (displayln x)
  (displayln "")
  x
   )

(define (add-row k j d circ)
  (define row (extract-row j d circ))
  (printf "row: ~a\n" row)
  (define mask (move k d row))
  (bvxor circ mask)
  )


;;;;; Clifford Gates ;;;;;
(define (S k n circ)
  (define d (* 2 n))
  (add-row k (+ k n) d circ)
  )

(define (H k n circ)
  (assert (and (<= 0 k)(< k n)))
  (define d (* 2 n))
  
  (define swap (bvxor (extract-row k d circ) (extract-row (+ k n) d circ)))
  (define mask (bvor (move k d swap) (move (+ k n) d swap)))
  (bvxor mask circ)
  )

(define (CNOT k j n circ)
  (assert (and (<= 0 k)(< k n)))
  (assert (and (<= 0 j)(< j n)))
  (assert (!(= j k)))
  
  (define d (* 2 n))
  (add-row (+ n k) (+ n j) d (add-row j k d circ))
  )

;;;; Equivalence ;;;;
(define (equivalent c1 c2)
  (bveq c1 c2))

;;;; Tests ;;;;
(define (tests)
  (test-1)
  (test-2)
  (test-3)
  (test-4))

(define (test-1)
  (define c1 (H 0 2(H 1 2(CNOT 0 1 2 (H 1 2(H 0 2 identity2))))))
  (define c2 (CNOT 1 0 2 identity2))
  (assert (equivalent c1 c2)))

(define (test-2)
  (define c1 (CNOT 0 1 2 (H 1 2 (H 0 2 identity2))))
  (define c2 (H 0 2 (H 1 2 (CNOT 1 0 2 identity2))))
  (assert (equivalent c1 c2)))

(define (test-3)
  (define c1 (CNOT 0 1 2 identity2))
  (define c2 (CNOT 1 0 2 identity2))
  (assert !(equivalent c1 c2)))

(define (test-4)
  (define c1 (CNOT 1 2 3 (CNOT 0 1 3 identity3)))
  (define c2 (CNOT 0 1 3 (CNOT 1 2 3 (CNOT 0 2 3 identity3))))
  (assert (equivalent c1 c2)))

;;;;;;;;;;;

(require rosette/lib/synthax)

; Synthesis
(define (synth-1)
  ; (define c1 (CNOT 1 2 3 (CNOT 0 1 3 identity3)))
  (define c1 (CNOT 1 (?? integer?) 3 (CNOT 0 1 3 identity3)))
  (define c2 (CNOT 0 1 3 (CNOT 1 2 3 (CNOT 0 2 3 identity3))))
  (assert (equivalent c1 c2)))


(define binding
  (synthesize #:forall (list)
              #:guarantee (synth-1)))
(print binding)
;(print-forms binding)


; Angelic exeuction
;(define-symbolic s1 integer?)
;(current-bitwidth 2)
;
;(define (synth-2)
;  ; (define c1 (CNOT 1 2 3 (CNOT 0 1 3 identity3)))
;  (define c1 (CNOT 1 s1 3 (CNOT 0 1 3 identity3)))
;  (define c2 (CNOT 0 1 3 (CNOT 1 2 3 (CNOT 0 2 3 identity3))))
;  (assert (equivalent c1 c2)))
;
;(define sol
;  (solve (synth-2)))
;(sat? sol)
;(evaluate s1 sol)
;(print sol)



;;;; Pretty print ;;;;;
;(define (print d circ)
;  (for ([i d])
;    (for ([j d])
;      ;(if (bveq (bv 1 1) (extract (+ (* i d) j) (+ (* i d) j) circ))
;       ;   (display "1 ")
;        ;  (display "0 ")
;         ; )
;      (displayln ((* i d) + j))
;      )
;    (displayln)
;    )
;  )
  