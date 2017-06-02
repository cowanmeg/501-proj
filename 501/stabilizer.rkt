#lang rosette
(require racket/match)


(current-bitwidth 36)

;; Hard coded identity matrices to start circuit with
(define identity2 (bv 33825 16))
(define identity3 (bv 9241421688590303745 36))

;;;; Manipulation functions ;;;;;
(define (extract-row r d circ)
  (assert (and (>= r 0) (< r d)))
  (extract (- (* d (+ r 1)) 1) (* d r) circ)
  )

(define (move r d row-elm)
  (assert (and (>= r 0) (< r d)))
 (bvshl
   (zero-extend row-elm (bitvector (* d d)))
   ;(bv (* dest d) (* d d)))
   (integer->bitvector (* r d) (bitvector (* d d))))
  )

(define (add-row k j d circ)
  (define mask (move k d (extract-row j d circ)))
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

; Synthesize inputs to gates 
(define-symbolic s1 s2 s3 s4 integer?)
(define (synth-1)
  ; (define c1 (CNOT 1 2 3 (CNOT 0 1 3 identity3)))
  (define c1 (CNOT s1 s2 3 (CNOT s3 s4 3 identity3)))
  (define c2 (CNOT 0 1 3 (CNOT 1 2 3 (CNOT 0 2 3 identity3))))
  (assert (equivalent c1 c2)))


(define binding
  (synthesize #:forall (list)
              #:guarantee (synth-1)))
(print binding)
(print-forms binding)


;;;;; Attempting to symbolify gates ;;;;;;;
(struct gate (id proc)
  #:transparent
  #:property prop:procedure
  (struct-field-index proc)
  )

(define (gate-arity gate)
  (match (procedure-arity gate)
    [(? integer? a) (- a 2)]
    [_ 2]))

(struct inst (gate in)
  #:transparent
  )

; Symbolic instruction - might be able to just use procedure-arity here
(define (inst* gate)
  (define (qubit-id*)
    (define-symbolic* q integer?)
    q)
  (inst gate (for/list ([i (gate-arity gate)]) (qubit-id*))))

; wrappers for gate procedure
(define s (gate 0 S))
(define h(gate 1 H))
(define cnot (gate 2 CNOT))
 
; Hardcoded for identity3 for now
(define (select-inst l goal n identity-matrix)
  (define (todo i circuit)
    (assert (and (>= i 0) (< i 3)))
    (define inst (case i
                   [(0) (inst* s)]
                   [(1) (inst* h)]
                   [(2) (inst* cnot)]))
    (define gate (gate-proc (inst-gate inst)))
    (define args (append (inst-in inst) (list n circuit)))
    (apply gate args))
      
  ;(foldl todo identity-matrix l)
  (assert (equivalent goal (foldl todo identity-matrix l))))


;;(define ll (list (inst h (list 0 3))))
;;(apply-insts ll)
    
  
; Synthesize a simpler circuit for this

(define-symbolic a x y z n integer?)
(define xs (take (list a x y z) n))
;(displayln xs)
(define (synth-2)
  (define cnot-3 (CNOT 0 1 3 (CNOT 1 2 3 (CNOT 0 2 3 identity3))))
  (select-inst xs cnot-3 3 identity3))

(define (synth-3)
  (define cnot-4-h (H 0 2(H 1 2(CNOT 0 1 2 (H 1 2(H 0 2 identity2))))))
  (select-inst xs cnot-4-h 2 identity2))

(define binding2
  ;(synthesize #:forall (list)
  (optimize #:minimize (list n)
              #:guarantee (synth-3)))
(print binding2)



