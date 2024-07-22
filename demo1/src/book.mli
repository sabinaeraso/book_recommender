open! Core
module Id = Int
module Title = String
module Subject = String

module Subjects : sig
  type t [@@deriving compare, sexp, hash]

  val create : string list -> t
  (* include T include Comparable.Make (T) *)
end

type t [@@deriving sexp, compare, hash]

(* include T include Comparable.Make (T) *)

val to_string : t -> string
val subjects : t -> Subject.t list
val id : t -> Id.t
val titel : t -> Title.t

(* functions: get_title , get_subjects , add_to_visited add_to_to_visit *)

(* val get_title : () val get_subjects : () val get_id : () *)
