open! Core
open Async

type t =
  { mutable stored_subjects : String.Set.t (* names of the subjects stored*)
  ; mutable size : int (* number of subjects stored*)
  }
[@@deriving sexp_of, fields ~getters]

let is_in_cache t subject = Set.mem t.stored_subjects subject
let check_limit t = t.size <= 1000

let create_cache () : t Deferred.t =
  let%bind text = Reader.file_lines "cache/all_subject_titles.txt" in
  return
    { stored_subjects = String.Set.of_list text; size = List.length text }
;;

let add_subject_to_all_subject_titles (subject : string) : unit Deferred.t =
  let%bind () =
    Writer.save_lines "cache/all_subject_titles.txt" [ subject ]
  in
  return ()
;;

let write_to_cache t subject =
  if check_limit t
  then (
    let valid_file =
      Or_error.try_with (fun () ->
        let raw_subject_page =
          Book_fetch.Fetcher.Subjects.fetch_sub ~limit:1000 subject
        in
        Writer.save ("cache/" ^ subject ^ ".txt") ~contents:raw_subject_page)
    in
    match valid_file with
    | Ok _ ->
      t.stored_subjects <- Set.add t.stored_subjects subject;
      t.size <- t.size + 1;
      add_subject_to_all_subject_titles subject
    | Error _ -> return ())
  else return ()
;;

(* need to ensure that the subject is already a created empty file before
   calling Writer.save*)

let get_from_cache t subject =
  Deferred.bind
    (if not (is_in_cache t subject)
     then (
       let%bind () = write_to_cache t subject in
       return ())
     else return ())
    ~f:(fun () ->
      let%bind text = Reader.file_contents ("cache/" ^ subject ^ ".txt") in
      return text)
;;

let write_file =
  let open Command.Let_syntax in
  Async_command.async
    ~summary:"Given a subject writes it to cache"
    [%map_open
      let subject = flag "subject" (required string) ~doc:"Subject" in
      fun () ->
        let%bind.Deferred sub = create_cache () in
        let%bind.Deferred text = get_from_cache sub subject in
        print_endline text;
        Deferred.return ()]
;;

let command =
  Command.group
    ~summary:"Parse Open Library Data"
    [ "write-to-cache", write_file ]
;;
