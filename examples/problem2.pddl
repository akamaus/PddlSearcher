(define (problem node-world1)
  (:domain node-world)
  (:init
     (at A) (near A B) (near B C) (near C B) (near B A) (placed-at O1 C) (placed-at O2 A))
  (:goal ((placed-at O1 B) (placed-at O2 B))))
