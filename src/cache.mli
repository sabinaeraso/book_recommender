open! Core
open Async

type t =
  { mutable stored_subjects : String.Set.t (* names of the subjects stored*)
  ; mutable size : int (* number of subjects stored*)
  }
[@@deriving sexp_of, fields ~getters]

(* creates a new cache instance by parsing the information in
   all_subject_titles.txt *)
val create_cache : unit -> t Deferred.t

(* Fetches a subject from Open Library, adds its raw string page to the cache
   directory as a .txt file, and adds it's name to all_subject_titles.txt and
   updates the current cache instance*)
val write_to_cache : t -> string -> bool Deferred.t

(* retrieves a subject's page from the cache and returns Some string if it's
   found. This will also add the subject to the cache calling write_to_cache
   if the subject is valid and not already there. If the subject is invalid
   or the limit of the cache has been reached, it returns None because it
   cannot add/retrieve this subject from the cache. *)
val get_from_cache : t -> string -> string option Deferred.t

(* command*)
val command : Command.t
