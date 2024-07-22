open! Core

module type Ordered = sig
  type t

  val compare : t -> t -> int
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

  (** [length h] returns the number of elements of [h] *)
  val length : t -> int

  (** [is_empty h] checks the emptiness of [h] *)
  val is_empty : t -> bool

  (** [add x h] adds a new element [x] in heap [h]; complexity O(log(n)). *)
  val add : t -> Value.t -> unit

  (** [minimum h] returns the minimum element of [h]; raises [Empty]
      when [h] is empty; complexity O(1) *)
  val minimum : t -> Value.t

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

module Make (X : Ordered) : S with type Value.t = X.t
