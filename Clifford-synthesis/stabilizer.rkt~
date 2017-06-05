#lang rosette/safe

(define circ (bv 33825 16))


;;;; Manipulation functions ;;;;;
(define (extract-row r d circ)
  (extract (- (* d (+ r 1)) 1) (* d r) circ))

(define (move p d row)
  (bvshl
   (zero-extend row (bitvector (* d d)))
   (bv (* d p) (* d d))))

(define (add-row k j d circ)
  (define mask (move k d (extract-row j d circ)))
  (bvxor circ mask)
  )


;;;;; Gates ;;;;;
(define (S k n circ)
  (define d (expt 2 n))
  (add-row k (+ k n) d circ)
  )

(define (H k n circ)
  (define d (expt 2 n))
  (define swap (bvxor (extract-row k d circ) (extract-row (+ k n) d circ)))
  (define mask (bvor (move k d swap) (move (+ k n) d swap)))
  (bvxor mask circ)
  )

(define (CNOT k j n circ)
  (define d (expt 2 n))
  (add-row (+ n k) (+ n j) d (add-row j k d circ))
  )

;;;; Equivalence ;;;;
(define (equivalent c1 c2)
  (bveq c1 c2))

;;;; Tests ;;;;
(define c1 (H 0 2(H 1 2(CNOT 0 1 2 (H 1 2(H 0 2 circ))))))
(define c2 (CNOT 1 0 2 circ))
(equivalent c1 c2)

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
  