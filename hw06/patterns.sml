fun map f []        = []
  | map f (x :: xs) = ((f x) :: map f xs)


fun xor (b1, b2) = (b1 and not b2) or (n2 and not b1)