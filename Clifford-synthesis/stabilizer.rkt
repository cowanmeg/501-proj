#lang rosette
(require rosette/lib/synthax)

(current-bitwidth 36)

;; Hard coded identity matrices representing an empty clifford circuit
(define identity1 (bv 9 4))
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
   (integer->bitvector (* r d) (bitvector (* d d))))
  )

(define (add-row k j d circ)
  (define mask (move k d (extract-row j d circ)))
  (bvxor circ mask)
  )


;;;;; Clifford Gates Definitions;;;;;
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


;;;;; Synthesizing Equivalent Circuits ;;;;;;;
; Gate = id and procedure
(struct gate (id proc)
  #:transparent
  #:property prop:procedure
  (struct-field-index proc)
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf port "~a" (gate-id self)))])

; Number of qubit inputs to a gate
(define (gate-arity gate)
  (match (procedure-arity gate)
    [(? integer? a) (- a 2)]
    [_ 2]))

; Instruction =  gate + inputs
(struct inst (gate in)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a" 
              `(,(inst-gate self) ,@(inst-in self))))])

; Instruction with symbolic inputs to a gate
(define (inst* gate)
  (define (qubit-id*)
    (define-symbolic* q integer?)
    q)
  (inst gate (for/list ([i (gate-arity gate)]) (qubit-id*))))

; Cliffords (use for nice printing)
(define s (gate "phase" S))
(define h(gate "hadamard" H))
(define cnot (gate "cnot" CNOT))
  
; Creates an instruction with a symbolic gate procedure
(define (sym-inst i)
  (case i
    [(0) (inst* s)]
    [(1) (inst* h)]
    [(2) (inst* cnot)]
    ))
; Creates a symbolic instruction for the special case of 1 qubit
(define (sym-inst-1 i)
  (case i
    [(0) (inst* s)]
    [(1) (inst* h)]
    ))

 
; Given a prgm - list of (potentially symbolic) instructions
;     goal - bv that want transformation to be equivalent to
;     n, identity-matrix: number of qubits in circuit + its starting empty circuit
; Determines if they prgm results in the goal matrix


(define (select-inst prgm goal n identity-matrix)
  (define (apply-instruction inst circuit)
    (define gate (gate-proc (inst-gate inst)))
    (define args (append (inst-in inst) (list n circuit)))
    (apply gate args))

  (assert (equivalent goal (foldl apply-instruction identity-matrix prgm))))



(define (find-optimal circuit size identity-matrix)
  (define-symbolic* a1 a2 a3 a4 a5 n integer?)
  (define prgm (take (list (sym-inst a1) (sym-inst a2) (sym-inst a3) (sym-inst a4)
                         (sym-inst a5)) n))
  
  (define binding
    (optimize #:minimize (list n)
              #:guarantee (select-inst prgm circuit size identity-matrix)))
  (evaluate prgm binding))

;;;;;;; Simple Tests ;;;;;;;;;;


;(define (synth-1)
  (define cnot-3 (CNOT 0 1 3 (CNOT 1 2 3 (CNOT 0 2 3 identity3))))
  ;(select-inst prgm cnot-3 3 identity3))

;(define (synth-2)
  (define cnot-4-h (H 0 2(H 1 2(CNOT 0 1 2 (H 1 2(H 0 2 identity2))))))
  ;(select-inst prgm cnot-4-h 2 identity2))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (synth-1-qubit x prgm)
  (select-inst prgm (bv x 4) 1 identity1))

(define (synth-2-qubit x prgm)
  (select-inst prgm (bv x 16) 2 identity2))

(define hist-1( make-vector 6))
(define hist-2( make-vector 6))

(define (run-1 x)
  (define-symbolic* a b c d e  n integer?)
  (define prgm (take (list (sym-inst-1 a) (sym-inst-1 b) (sym-inst-1 c) (sym-inst-1 d)
                         (sym-inst-1 e)) n))
  (define binding2 
  ;(synthesize #:forall (list)
    (optimize #:minimize (list n)
              #:guarantee (synth-1-qubit x prgm)))
  (if (sat? binding2)
      (begin (define idx (evaluate n binding2))
            (vector-set! hist-1 idx (+ 1 (vector-ref hist-1 idx)))
            idx)
     "unsat")
      ;(display binding2)
      ;(evaluate prgm binding2)
  )

(define (run x)
  (define-symbolic* a1 a2 a3 a4 a5 a6 a7 a8  n integer?)
  (define prgm (take (list (sym-inst a1) (sym-inst a2) (sym-inst a3) (sym-inst a4)
                         (sym-inst a5) (sym-inst a6) (sym-inst a7) (sym-inst a8)) n))
  (define binding2 
  ;(synthesize #:forall (list)
    (optimize #:minimize (list n)
              #:guarantee (synth-2-qubit x prgm)))
  (cond [(sat? binding2)
      (begin (define idx (evaluate n binding2))
             (vector-set! hist-2 idx (+ 1 (vector-ref hist-2 idx))))]
      ;(evaluate prgm binding2)
  ))

;(map run-1 (for/list ([i 16]) i))
;33825
;(map run (for/list ([x (in-range 33000 33200)]) x))
