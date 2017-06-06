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

; Symbolic instruction
(define (inst* gate)
  (define (qubit-id*)
    (define-symbolic* q integer?)
    q)
  (inst gate (for/list ([i (gate-arity gate)]) (qubit-id*))))
  

; Clifford gates - don't really need the id...
(define s (gate "phase" S))
(define h(gate "hadamard" H))
(define cnot (gate "cnot" CNOT))
 
; 
(define (select-inst prgm goal n identity-matrix)
  (define (apply-instruction inst circuit)
    (define gate (gate-proc (inst-gate inst)))
    (define args (append (inst-in inst) (list n circuit)))
    (apply gate args))

  (assert (equivalent goal (foldl apply-instruction identity-matrix prgm))))

  
; Synthesize a simpler circuit for this

(define (sym-inst i)
  (case i
    [(0) (inst* s)]
    [(1) (inst* h)]
    [(2) (inst* cnot)]))


(define-symbolic a b c d n integer?)
(define prgm (take (list (sym-inst a) (sym-inst b) (sym-inst c) (sym-inst d)) n))
(define (synth-2)
  (define cnot-3 (CNOT 0 1 3 (CNOT 1 2 3 (CNOT 0 2 3 identity3))))
  (select-inst prgm cnot-3 3 identity3))

(define (synth-3)
  (define cnot-4-h (H 0 2(H 1 2(CNOT 0 1 2 (H 1 2(H 0 2 identity2))))))
  (select-inst prgm cnot-4-h 2 identity2))

(define binding2
  ;(synthesize #:forall (list)
  (optimize #:minimize (list n)
              #:guarantee (synth-2)))
(evaluate prgm binding2)



