open! Core

type t =
  { mutable stored_subjects : String.Set.t (* names of the subjects stored*)
  ; mutable size : int (* number of subjects stored*)
  }
[@@deriving sexp_of, fields ~getters]

val create_cache : unit -> t
val write_to_cache : t -> string -> unit
val get_from_cache : t -> string -> string
val command : Command.t
