open! Core

module type Ordered = sig
  type t [@@deriving sexp]

  val compare : t -> t -> int
  val key : t -> string
end

exception Empty

module type S = sig
  module Value : sig
    type t
  end

  (** Type of priority queues. *)
  type t [@@deriving sexp]

  (** [create ~dummy c] creates a new heap, with initial capacity of [c].
      The value [dummy] is used to fill unused cells of the internal array.
      Note: [dummy] can still be used as a regular value in the queue. *)
  val create : dummy:Value.t -> int -> t


  val data : t -> Value.t array

  (** [length h] returns the number of elements of [h] *)
  val length : t -> int

  (** [is_empty h] checks the emptiness of [h] *)
  val is_empty : t -> bool

  val index_map : t -> int Core.String.Table.t


  (** [add x h] adds a new element [x] in heap [h]; complexity O(log(n)). *)
  val add : t -> Value.t -> unit

  val find_index : t -> key:string -> int
  val heapify_after_update_at_index : Value.t -> t -> int -> unit
  (** [minimum h] returns the minimum element of [h]; raises [Empty]
      when [h] is empty; complexity O(1) *)
  val minimum : t -> Value.t

  val movedown : t -> int -> int -> Value.t -> unit
  (** [remove h] removes the minimum element of [h]; raises [Empty]
      when [h] is empty; complexity O(log(n)). *)
  val remove : t -> unit

  (** [pop_minimum h] removes the minimum element of [h] and returns it;
      raises [Empty] when [h] is empty; complexity O(log(n)). *)
  val pop_minimum : t -> Value.t

  (** [remove_and_add x h] removes the minimum element of [h] and adds [x];
      complexity O(log(n)). More efficient than calling [remove]
      and [add]. *)
  val remove_and_add : t -> Value.t -> unit

  (** usual iterators; elements are presented in arbitrary order *)
  val iter : (Value.t -> unit) -> t -> unit

  val fold : (Value.t -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make (X : Ordered) = struct
  (* The heap is encoded in the array [data], where elements are stored from
     [0] to [size - 1]. From an element stored at [i], the left (resp. right)
     subtree, if any, is rooted at [2*i+1] (resp. [2*i+2]). *)
  module Value = X

  type t =
    { mutable size : int
    ; index_map : int String.Table.t
    ; mutable data : X.t array
    ; dummy : X.t
    ; min_cap : int (* minimal capacity, as given initially *)
    }
  [@@deriving sexp]
  (* invariant 0 <= size <= length data *)
  (* invariant data[size..] only contains dummy *)

  let create ~dummy n =
    if n < 0 || n > Sys.max_array_length then invalid_arg "create";
    let n = max 16 n in
    { size = 0
    ; index_map = Hashtbl.create (module String)
    ; data = Array.create ~len:n dummy
    ; dummy
    ; min_cap = n
    }
  ;;

  let data h = h.data 

  let length h = h.size
  let is_empty h = h.size = 0

let index_map h = h.index_map



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
        Hashtbl.set h.index_map ~key:(X.key d.(fi)) ~data:i;
        moveup fi)
      else (d.(i) <- x;
      Hashtbl.set h.index_map ~key:(X.key x) ~data:i;
      )
    in
    moveup n;
    h.size <- n + 1
  ;;

  let find_index heap ~key =
    let index_map = heap.index_map in
    match Hashtbl.find index_map key with
    | None -> failwith "this book is not  in the index map"
    | Some index -> index
  ;;


  let minimum h =
    if h.size <= 0 then raise Empty;
    h.data.(0)
  ;;

  let rec movedown h n i x =
    let d  = h.data in
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
        Hashtbl.set h.index_map ~key: (X.key (d.(j))) ~data:i ; 
        movedown h n j x)
      else (d.(i) <- x; 
      Hashtbl.set h.index_map ~key: (X.key x) ~data:i ;) )
    else (d.(i) <- x ; 
    Hashtbl.set h.index_map ~key: (X.key x) ~data:i ;)
  ;;

  let rec heapify_after_update_at_index updated_book heap index_to_move =
    if equal 0 index_to_move
    then ( Array.set heap.data 0 updated_book;
    Hashtbl.set heap.index_map ~key: (X.key updated_book) ~data:0 ;
    movedown heap (heap.size) 0 updated_book;
     )
    else (
      let parent_index = (index_to_move - 1) / 2 in
      Hashtbl.set heap.index_map ~key: (X.key (Array.get heap.data parent_index)) ~data:index_to_move ;
      Hashtbl.set heap.index_map ~key: (X.key (Array.get heap.data index_to_move)) ~data:parent_index ;
      Array.swap heap.data index_to_move parent_index ; 
      heapify_after_update_at_index updated_book heap parent_index)
  ;;

  let remove h =
    if h.size <= 0 then raise Empty;
    let n = h.size - 1 in
    h.size <- n;
    let d = h.data in
    let x = d.(n) in
    d.(n) <- h.dummy;
    movedown h n 0 x;
    if 4 * h.size < Array.length h.data then shrink h
  ;;

  let remove_and_add h x =
    if h.size = 0 then add h x else movedown h h.size 0 x
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
