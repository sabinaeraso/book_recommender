open! Core
open Async

type t =
  { mutable stored_subjects : String.Set.t (* names of the subjects stored*)
  ; mutable size : int (* number of subjects stored*)
  }
[@@deriving sexp_of, fields ~getters]

val create_cache : unit -> t Deferred.t
val write_to_cache : t -> string -> bool Deferred.t
val get_from_cache : t -> string -> string option Deferred.t
val command : Command.t
