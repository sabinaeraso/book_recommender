open! Core
open Async

module Cache_item : sig
  type t =
    { title : string
    ; work_count : int
    }
  [@@deriving sexp, fields ~getters, compare]

  val create : title:string -> work_count:int -> t

  module Binary_heap : Binary_heap.S with type Value.t = t
end

type t =
  { mutable stored_subjects : Cache_item.Binary_heap.t
  ; mutable size : int
  ; path : string
  }
[@@deriving sexp, fields ~getters]

(* creates a new cache instance by parsing the information in
   all_subject_titles.txt *)
val create_cache : string -> t Deferred.t

(* retrieves a subject's page from the cache and returns Some string if it's
   found. This will also add the subject to the cache calling write_to_cache
   if the subject is valid and not already there. If the subject is invalid
   or the limit of the cache has been reached, it returns None because it
   cannot add/retrieve this subject from the cache. *)
val get_from_cache : t -> string -> string Deferred.t

(* command*)
val command : Command.t
