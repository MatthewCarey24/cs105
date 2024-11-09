(*********************** COMP 105 ML ASSIGNMENT I ****************************)

(* String Builders for Unit Tests *)
val int_list_toString = Unit.listString Unit.intString
val int_pair_toString = Unit.pairString Unit.intString Unit.intString
val bool_pair_toString = Unit.pairString Unit.boolString Unit.boolString
val list_pair_toString = Unit.listString int_pair_toString
val list_boolpair_toString = Unit.listString bool_pair_toString

(***** Problem 1 *****)

(* mynull takes a list of any type and returns whether or not the list is 
   null *)
fun mynull [] = true
  | mynull _  = false
  
        (* Unit Tests *)
        val () =
            Unit.checkAssert "[] is null"
            (fn () => mynull [])

        val () = 
            Unit.checkAssert "x :: xs is not null"
            (fn () =>  not (mynull [1, 2]))

        val () = Unit.reportWhenFailures ()


(***** Problem 2 ****)
(* takes a list of any type and returns the list in reverse order *)
fun reverse [] = []
  | reverse xs = foldl (fn (y, ys) => y :: ys) [] xs


        (* Unit Tests *)
        val () =
           Unit.checkExpectWith int_list_toString "reversing empty"
           (fn () => reverse [])
           []

        val () = 
           Unit.checkExpectWith int_list_toString "reverse non-empty"
           (fn () => reverse [1, 2, 3])
           [3, 2, 1]


(* takes a list of integers and returns the minimum element *)
fun minlist [] = raise Match
  | minlist (x :: xs) = foldr (fn (x, min) => Int.min (x, min)) x (x :: xs)

        (* Unit Tests *)
        val () =
           Unit.checkExpectWith Unit.intString "smallest of singleton"
           (fn () => minlist [1])
           1
        val () = 
            Unit.checkExnWith Unit.intString "smallest of empty"
            (fn () => minlist [])
        val () =
           Unit.checkExpectWith Unit.intString "smallest of positives"
           (fn () => minlist [2, 5, 43, 3])
           2
        val () = 
           Unit.checkExpectWith Unit.intString "smallest of reals"
           (fn () => minlist [1, 0, ~4, 45])
           ~4



(***** Problem 3 ****)
exception Mismatch

(* zip takes 2 lists of equal length and returns 1 list of pairs of 
   corresponding elements of the input lists *)
fun zip ([], [])           = []
  | zip ([], _)            = raise Mismatch
  | zip (_, [])            = raise Mismatch
  | zip (x :: xs, y :: ys) = (x, y) :: (zip (xs, ys))


        (* Unit Tests *)
        val () =
            Unit.checkExpectWith list_pair_toString "zip on integer lists"
            (fn () => zip ([1, ~2, 4], [9, 12, 0]))
            [(1, 9), (~2, 12), (4, 0)]

        val () =
            Unit.checkExnWith list_pair_toString "zip 1st too short"
            (fn () => zip ([1, ~2], [9, 12, 0]))

        val () =
            Unit.checkExnWith list_pair_toString "zip 2nd too short"
            (fn () => zip ([1, ~2], [9]))


(***** Problem 4 ****)

(* pairfoldrEq takes a function f that accepts three inputs, an initial value 
   init, and a pair of equal-length lists (xs, ys), returning a combined result
   by folding the lists together or raising a Mismatch error if the lists are 
   not of equal length *)
fun pairfoldrEq _ init ([], [])           = init
  | pairfoldrEq _ _ (_, [])               = raise Mismatch 
  | pairfoldrEq _ _ ([], _)               = raise Mismatch 
  | pairfoldrEq f init (x :: xs, y :: ys) = f 
                                            (x, y, pairfoldrEq f init (xs, ys))

        (* Unit Tests *)
        fun add_pairs (a, b, acc) = a + b + acc

        val () =
           Unit.checkExpectWith Unit.intString "double fold empty lists"
           (fn () => pairfoldrEq add_pairs 0 ([], []))
           0
        val () =
           Unit.checkExnWith Unit.intString 
           "double fold on unequal length lists"
           (fn () => pairfoldrEq add_pairs 0 ([1, 2], [3])) 

(* ziptoo takes 2 lists of equal length and returns 1 list of pairs of 
   corresponding elements of the input lists *)
fun ziptoo ([], [])           = []
  | ziptoo ([], _)            = raise Mismatch
  | ziptoo (_, [])            = raise Mismatch
  | ziptoo (xs, ys) = pairfoldrEq (fn (x, y, c) => (x, y) :: c) [] (xs, ys)

        (* Unit Tests *)
        val () =
            Unit.checkExpectWith list_pair_toString "ziptoo on integer lists"
            (fn () => ziptoo ([1, ~2, 4], [9, 12, 0]))
            [(1, 9), (~2, 12), (4, 0)]

        val () =
            Unit.checkExnWith list_pair_toString "ziptoo 1st too short"
            (fn () => ziptoo ([1, ~2], [9, 12, 0]))

        val () =
            Unit.checkExnWith list_pair_toString "ziptoo 2nd too short"
            (fn () => ziptoo ([1, ~2], [9]))



(**** Problem 5 ****)

(* concat takes a list of 'a lists and returns a single list in order*)
fun concat [] = []
  | concat (xs :: ys) = xs @ (concat ys)

        (* Unit Tests *)
        val () = 
           Unit.checkExpectWith int_list_toString "concat empty list"
           (fn () => concat [])
           []
        val () = 
           Unit.checkExpectWith int_list_toString "concat empty list list"
           (fn () => concat [[]])
           []
        val () = 
           Unit.checkExpectWith int_list_toString "concat intlist1"
           (fn () => concat [[1], [2, 3, 4], [], [5, 6]])
           [1, 2, 3, 4, 5, 6]
        val () = 
           Unit.checkExpectWith int_list_toString "concat intlist2"
           (fn () => concat [[1], [5, 3, 4], [32], []])
           [1, 5, 3, 4, 32]


(********************************** Part B ***********************************)

(**** Problem 6 ****)
datatype ordsx 
  = BOOL of bool
  | NUM  of int
  | SYM  of string
  | SXS  of ordsx list

(* numbersSx takes a list of numbers and returns it an s-expression *)
fun numbersSx num_list = SXS (List.map NUM num_list)

(* sxString takes an s-expression and returns it in a string format *)
fun sxString (SYM s)   = s
  | sxString (NUM n)   = Unit.intString n
  | sxString (BOOL b)  = if b then "true" else "false"
  | sxString (SXS sxs) = "(" ^ String.concatWith " " (map sxString sxs) ^ ")"

        (* Unit Test *)
        val () =
        Unit.checkExpectWith sxString "sexp list"
        (fn () => numbersSx [1, 2, 3])
        (SXS [NUM 1, NUM 2, NUM 3])

        val () =
        Unit.checkExpectWith sxString "Empty sexp"
        (fn () => numbersSx [])
        (SXS [])

(* flattenSyms takes any s-expression and returns a list of any symbol in it *)
fun flattenSyms (SYM s)    = [s]
  | flattenSyms (SXS xs) = List.concat (map flattenSyms xs)
  | flattenSyms _          = []

        (* Unit Test *)
        val () =
        Unit.checkExpectWith String.concat "flatten 1 symbol"
        (fn () => flattenSyms (SYM "x"))
        ["x"]

        val () =
        Unit.checkExpectWith String.concat "flatten s-expression"
        (fn () => flattenSyms (SXS [NUM 1, SYM "x", SYM "y"]))
        ["x", "y"]

        val () =
        Unit.checkExpectWith String.concat "flatten other forms"
        (fn () => flattenSyms (NUM 1))
        []
