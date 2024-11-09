  (val y 0)                   
  (val f (lambda () y))       
  (val y 1)                     
  (if (= (f) 0) 'new-semantics 'old-semantics)
