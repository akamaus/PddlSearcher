(define (domain node-world)
  (:action move
           :precondition (and (at ?x1) (near ?x1 ?x2))
           :effect (and (not (at ?x1)) (at ?x2)))
  (:action grab
           :precondition (and (at ?x1) (placed-at ?obj ?x1) (not (curring ?any)))
           :effect (and (not (placed-at ?obj ?x1)) (curring ?obj)))
  (:action drop
           :precondition (and (at ?x1) (curring ?obj))
           :effect (and (not (curring ?obj)) (placed-at ?obj ?x1))))
