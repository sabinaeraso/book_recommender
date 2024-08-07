open! Core
open Async

module Cache_item = struct
  type t =
    { title : string
    ; work_count : int
    }
  [@@deriving sexp, fields ~getters, compare]

  let compare_by_work_count =
    Comparable.lift Int.compare ~f:(fun cache_item -> cache_item.work_count)
  ;;

  let create ~title ~work_count = { title; work_count }

  module T = struct
    type nonrec t = t [@@deriving sexp]

    let compare = compare_by_work_count
    let key = title
  end

  module Binary_heap = Binary_heap.Make (T)
end

type t =
  { mutable stored_subjects : Cache_item.Binary_heap.t
  ; mutable size : int
  }
[@@deriving sexp, fields ~getters]

let format_filename (subject : string) =
  String.lowercase subject
  |> String.split_on_chars ~on:[ ' ' ]
  |> String.concat ~sep:"_"
;;

let is_in_cache t subject =
  match
    Cache_item.Binary_heap.find_index
      t.stored_subjects
      ~key:(format_filename subject)
  with
  | Some _ -> true
  | None -> false
;;

let limit = 40
let below_limit t = t.size < limit

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
  let empty_heap =
    Cache_item.Binary_heap.create
      ~dummy:(Cache_item.create ~title:"dummy" ~work_count:0)
      limit
  in
  let stored_subjects =
    List.fold text ~init:empty_heap ~f:(fun heap line ->
      let name_count = String.split line ~on:' ' in
      let new_cache_item =
        Cache_item.create
          ~title:(List.nth_exn name_count 0)
          ~work_count:(Int.of_string (List.nth_exn name_count 1))
      in
      Cache_item.Binary_heap.add heap new_cache_item;
      heap)
  in
  return { stored_subjects; size = List.length text }
;;

let add_subject_and_work_count_to_all_subject_titles
  (cache_item : Cache_item.t)
  : unit Deferred.t
  =
  let%bind current_subs = Reader.file_lines "cache/all_subject_titles.txt" in
  let%bind () =
    Writer.save_lines
      "cache/all_subject_titles.txt"
      (current_subs
       @ [ cache_item.title ^ " " ^ Int.to_string cache_item.work_count ])
  in
  return ()
;;

let update_cache_file
  (heap_of_cache : Cache_item.Binary_heap.t)
  : unit Deferred.t
  =
  let%bind () = Writer.save_lines "cache/all_subject_titles.txt" [] in
  Cache_item.Binary_heap.iter (fun item -> ) heap_of_cache
  let%bind () =
    Writer.save_lines
      "cache/all_subject_titles.txt"
      ([ cache_item.title ^ " " ^ Int.to_string cache_item.work_count ])
  in
  return ()
;;

let replace_min_subject t (cache_item : Cache_item.t) =
  Core.print_endline "entered replacement subjects";
  let current_min = Cache_item.Binary_heap.minimum t.stored_subjects in
  let current_min_title = current_min.title in
  let current_min_count = current_min.work_count in
  if cache_item.work_count > current_min_count
  then (
    let _ = Cache_item.Binary_heap.pop_minimum t.stored_subjects in
    Core.print_s [%sexp (current_min : Cache_item.t)];
    Core.print_endline "replaced with";
    Core.print_s [%sexp (cache_item : Cache_item.t)];
    Sys_unix.remove ("cache/" ^ current_min_title ^ ".txt");
    t.size <- t.size - 1)
  else ()
;;

let write_to_cache t raw_subject_page formatted_subject =
  let cache_item =
    Cache_item.create
      ~title:formatted_subject
      ~work_count:(Page_parser.Subject_page.get_work_count raw_subject_page)
  in
  if equal t.size limit then replace_min_subject t cache_item;
  if below_limit t
  then (
    match%bind
      Deferred.Or_error.try_with (fun () ->
        Page_parser.parse_from_string raw_subject_page |> Deferred.return)
      |> Deferred.Or_error.ignore_m
    with
    | Error _ -> return ()
    | Ok () ->
      let%bind () =
        Writer.save
          ("cache/" ^ formatted_subject ^ ".txt")
          ~contents:raw_subject_page
      in
      Cache_item.Binary_heap.add t.stored_subjects cache_item;
      t.size <- t.size + 1;
      let%bind () =
        add_subject_and_work_count_to_all_subject_titles cache_item
      in
      return ())
  else return ()
;;

(* should only hit this last case if the cache is full and the subject ur
   looking for has less works than the min of the heap*)

let get_from_cache t subject =
  let formatted_subject = format_filename subject in
  if is_in_cache t formatted_subject
  then (
    let%bind text =
      Reader.file_contents ("cache/" ^ formatted_subject ^ ".txt")
    in
    return text)
  else (
    let raw_subject_page =
      Book_fetch.Fetcher.Subjects.fetch_sub ~limit:1000 subject
    in
    if properly_formatted_subject formatted_subject
    then (
      let%bind () = write_to_cache t raw_subject_page formatted_subject in
      return raw_subject_page)
    else return raw_subject_page)
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
        print_s [%sexp (text : string)];
        Deferred.return ()]
;;

let command =
  Command.group
    ~summary:"Parse Open Library Data"
    [ "write-to-cache", write_file ]
;;
