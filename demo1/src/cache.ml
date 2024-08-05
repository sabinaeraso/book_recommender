open! Core
open Async

type t =
  { stored_subjects : Set.Make(String).t (* names of the subjects stored*)
  ; size : int (* number of subjects stored*)
  }

let is_in_cache t subject = Set.mem t.stored_subjects subject

let write_to_cache t subject = 
  Deferred.bind (Reader.file_contents (subject ^ ".json"))
    ~f:(fun text ->
       Writer.save filename ~contents:(String.uppercase text));;
val uppercase_file : string -> unit Deferred.t = <fun>
uppercase_file "test.txt";;
- : unit = ()
Reader.file_contents "test.txt";;
- : string = "THIS IS ONLY A TEST."

;;

let get_from_cache t subject = 
;;

let get_or_write_to_cache t subject = 
if is_in_cache 
  then get_from_cache t subject 
else (* file fetch and call to api, *)
 write_to_cache t subject

;;
