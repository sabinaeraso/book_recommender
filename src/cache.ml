open! Core
open Async

type t =
  { mutable stored_subjects : String.Set.t
  ; mutable size : int
  }
[@@deriving sexp_of, fields ~getters]

let format_filename (subject : string) =
  String.lowercase subject
  |> String.split_on_chars ~on:[ ' ' ]
  |> String.concat ~sep:"_"
;;

let is_in_cache t subject =
  Set.mem t.stored_subjects (format_filename subject)
;;

let below_limit t = t.size <= 1000

let properly_formatted_subject (subject : string) =
  let banned_symbols =
    [ "#"
    ; "%"
    ; "&"
    ; "{"
    ; "}"
    ; "\\"
    ; "<"
    ; ">"
    ; "*"
    ; "?"
    ; "/"
    ; "$"
    ; "!"
    ; "'"
    ; "\""
    ; ":"
    ; "@"
    ; "+"
    ; "`"
    ; "|"
    ; "="
    ]
  in
  List.fold_until
    banned_symbols
    ~finish:(fun _b -> true)
    ~init:true
    ~f:(fun state word ->
      let lower_subject = String.lowercase subject in
      if String.is_substring ~substring:word lower_subject
         || String.equal word lower_subject
      then Stop false
      else Continue state)
;;

let create_cache () : t Deferred.t =
  let%bind text = Reader.file_lines "cache/all_subject_titles.txt" in
  return
    { stored_subjects = String.Set.of_list text; size = List.length text }
;;

let add_subject_to_all_subject_titles (subject : string) : unit Deferred.t =
  let%bind current_subs = Reader.file_lines "cache/all_subject_titles.txt" in
  let%bind () =
    Writer.save_lines
      "cache/all_subject_titles.txt"
      (current_subs @ [ subject ])
  in
  return ()
;;

let write_to_cache t subject =
  let formatted_subject = format_filename subject in
  if below_limit t
  then (
    let valid_file =
      Or_error.try_with (fun () ->
        let raw_subject_page =
          Book_fetch.Fetcher.Subjects.fetch_sub ~limit:1000 subject
        in
        let _parsed = Page_parser.parse_from_string raw_subject_page in
        Writer.save
          ("cache/" ^ formatted_subject ^ ".txt")
          ~contents:raw_subject_page)
    in
    match valid_file with
    | Ok file_has_been_written ->
      let%bind () = file_has_been_written in
      t.stored_subjects <- Set.add t.stored_subjects formatted_subject;
      t.size <- t.size + 1;
      let%bind () = add_subject_to_all_subject_titles formatted_subject in
      return true
    | Error _ -> return false)
  else return false
;;

let get_from_cache t subject =
  let formatted_subject = format_filename subject in
  if is_in_cache t formatted_subject
  then (
    let%bind text =
      Reader.file_contents ("cache/" ^ formatted_subject ^ ".txt")
    in
    return (Some text))
  else if below_limit t && properly_formatted_subject formatted_subject
  then (
    let%bind wrote_to_cache = write_to_cache t subject in
    if wrote_to_cache
    then (
      let%bind text =
        Reader.file_contents ("cache/" ^ formatted_subject ^ ".txt")
      in
      return (Some text))
    else return None)
  else return None
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
        print_s [%sexp (text : string option)];
        Deferred.return ()]
;;

let command =
  Command.group
    ~summary:"Parse Open Library Data"
    [ "write-to-cache", write_file ]
;;
