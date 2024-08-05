open! Core
open Async

type t =
  { mutable stored_subjects : String.Set.t (* names of the subjects stored*)
  ; _size : int (* number of subjects stored*)
  }

let is_in_cache t subject = Set.mem t.stored_subjects subject

let write_to_cache t subject =
  t.stored_subjects <- Set.add t.stored_subjects subject;
  (let raw_subject_page = Book_fetch.Fetcher.Subjects.fetch_sub subject in
   Writer.save ("cache/" ^ subject ^ ".txt") ~contents:raw_subject_page)
  |> ignore
;;

(* need to ensure that the subject is already a created empty file before
   calling Writer.save*)

let get_from_cache t subject =
  let open Deferred.Let_syntax in
  if not (is_in_cache t subject) then write_to_cache t subject;
  let%map text = Reader.file_contents (subject ^ ".txt") in
  text
;;

let write_file =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Given a subject writes it to cache"
    [%map_open
      let subject = flag "subject" (required string) ~doc:"Subject" in
      fun () ->
        let fetched_file = Book_fetch.Fetcher.Subjects.fetch_sub subject in
        let sub =
          { stored_subjects = String.Set.of_list [ "hi" ]; _size = 0 }
        in
        get_from_cache sub fetched_file |> ignore]
;;

let command =
  Command.group
    ~summary:"Parse Open Library Data"
    [ "write-to-cache", write_file ]
;;