(* open! Core
open Async

type t =
  { mutable stored_subjects : Set.Make(String).t
      (* names of the subjects stored*)
  ; size : int (* number of subjects stored*)
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
  if not (is_in_cache t subject) then write_to_cache t subject;
  let%bind text = Reader.file_contents (subject ^ ".txt") in
  return text
;;

let command =
  Command.group
    ~summary:"Parse Open Library Data"
    [ "books-from-subject", get_from_cache ]
;;
