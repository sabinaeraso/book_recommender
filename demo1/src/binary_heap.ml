open! Core

module type Ordered = sig
  type t

  val compare : t -> t -> int
end

exception Empty

module Make (X : Ordered) = struct
  (* The heap is encoded in the array [data], where elements are stored from
     [0] to [size - 1]. From an element stored at [i], the left (resp. right)
     subtree, if any, is rooted at [2*i+1] (resp. [2*i+2]). *)

  type t =
    { mutable size : int
    ; mutable data : X.t array
    ; dummy : X.t
    ; min_cap : int (* minimal capacity, as given initially *)
    }
  (* invariant 0 <= size <= length data *)
  (* invariant data[size..] only contains dummy *)

  let create ~dummy n =
    if n < 0 || n > Sys.max_array_length then invalid_arg "create";
    let n = max 16 n in
    { size = 0; data = Array.create ~len:n dummy; dummy; min_cap = n }
  ;;

  let length h = h.size
  let is_empty h = h.size = 0

  (* [enlarge] doubles the size of [data] *)
  let enlarge h =
    let n = h.size in
    assert (n > 0 && n = Array.length h.data);
    let n' = min (2 * n) Sys.max_array_length in
    if n' = n then failwith "maximum capacity reached";
    let d = h.data in
    let d' = Array.create ~len:n' h.dummy in
    Array.blit ~src:d ~src_pos:0 ~dst:d' ~dst_pos:0 ~len:n;
    h.data <- d'
  ;;

  let shrink h =
    let n = Array.length h.data in
    let n' = max h.min_cap (n / 2) in
    assert (h.size <= n' && n' <= n);
    if n' < n
    then (
      let d = h.data in
      let d' = Array.create ~len:n' h.dummy in
      Array.blit ~src:d ~src_pos:0 ~dst:d' ~dst_pos:0 ~len:h.size;
      h.data <- d')
  ;;

  let add h x =
    let n = h.size in
    if equal n (Array.length h.data) then enlarge h;
    let d = h.data in
    let rec moveup i =
      let fi = (i - 1) / 2 in
      if i > 0 && X.compare d.(fi) x > 0
      then (
        d.(i) <- d.(fi);
        moveup fi)
      else d.(i) <- x
    in
    moveup n;
    h.size <- n + 1
  ;;

  let minimum h =
    if h.size <= 0 then raise Empty;
    h.data.(0)
  ;;

  let rec movedown d n i x =
    let j = (2 * i) + 1 in
    if j < n
    then (
      let j =
        let j' = j + 1 in
        if j' < n && X.compare d.(j') d.(j) < 0 then j' else j
      in
      if X.compare d.(j) x < 0
      then (
        d.(i) <- d.(j);
        movedown d n j x)
      else d.(i) <- x)
    else d.(i) <- x
  ;;

  let remove h =
    if h.size <= 0 then raise Empty;
    let n = h.size - 1 in
    h.size <- n;
    let d = h.data in
    let x = d.(n) in
    d.(n) <- h.dummy;
    movedown d n 0 x;
    if 4 * h.size < Array.length h.data then shrink h
  ;;

  let remove_and_add h x =
    if h.size = 0 then add h x else movedown h.data h.size 0 x
  ;;

  let pop_minimum h =
    let m = minimum h in
    remove h;
    m
  ;;

  let iter f h =
    let d = h.data in
    for i = 0 to h.size - 1 do
      f d.(i)
    done
  ;;

  let fold f h x0 =
    let n = h.size in
    let d = h.data in
    let rec foldrec x i =
      if i >= n then x else foldrec (f d.(i) x) (succ i)
    in
    foldrec x0 0
  ;;
end
