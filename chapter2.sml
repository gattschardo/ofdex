signature CHAPTER_2 =
sig

(* Exercise 2.1 *)
(* returns all suffixes of l *)
(* time: O(n), space: O(n) *)
val suffixes : 'a list -> 'a list list

end

structure Chapter_2 : CHAPTER_2 =
struct

(* returns all suffixes of l *)
(* all actual data nodes in result are shared,
 * only result list itself is consed
 *)
fun suffixes [] = []
  | suffixes (l as (_::rest)) = l :: (suffixes rest)
end

signature ORDERING =
sig
type t

val lt : t -> t -> bool
val eql : t -> t -> bool
val lte : t -> t -> bool
end

signature SET =
sig
type elem
type t

val empty : t
val insert : elem -> t -> t
val member : elem -> t -> bool
val from_list : elem list -> t
end

functor BST(structure elem : ORDERING) :> SET
where type elem = elem.t =
struct
type elem = elem.t
datatype t = E | T of t * elem * t

val empty = E

local
(* Exercise 2.2 *)
(* Keep a candidate c to compare for equality when reaching the bottom of the
 * tree, to cut down the total number of comparisions (same below at insert)
 *)
fun member_cand x c E = elem.eql x c
  | member_cand x c (T (l, y, r)) =
      if elem.lt x y then
        member_cand x c l
      else
        member_cand x y r

in
fun member _ E = false
  | member x (t as T (_, y, _)) = member_cand x y t
end

local
(* Exercise 2.3 *)
(* Copying of nodes happens on demand only, not when an element that is already
 * present is inserted again
 *)
(* Exercise 2.4 *)
(* Combine copy avoidance (2.3) and comparison reduction (2.2) *)
exception Copy of t
exception Keep
fun maybe_insert_cand x c E =
      if elem.eql x c then
        raise Keep
      else
        raise (Copy (T (E, x, E)))
  | maybe_insert_cand x c (t as T (l, y, r)) =
      if elem.lt x y then
        (maybe_insert_cand x c l handle
          Copy l' => raise (Copy (T (l', y, r))))
      else
        (maybe_insert_cand x y r handle
          Copy r' => raise (Copy (T (l, y, r'))))

in
  fun insert x E = T (E, x, E)
    | insert x (t as T (_, y, _)) =
        maybe_insert_cand x y t handle
          Copy t' => t' | Keep => t
end

fun from_list l =
  foldl (fn (x, r) => insert x r) empty l
end

functor mkOrdering(type t val compare : t * t -> order) =
struct

type t = t

fun lt x y = (compare (x, y) = LESS)

fun eql x y = (compare (x, y) = EQUAL)

fun lte x y = not (compare (x, y) = GREATER)
end

structure IO = mkOrdering(type t = int val compare= Int.compare)
structure IT = BST(structure elem = IO)

(* vim: se ai et sw=2 ts=2: *)
