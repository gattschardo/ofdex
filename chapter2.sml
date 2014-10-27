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

(* vim: se ai et sw=2 ts=2: *)
