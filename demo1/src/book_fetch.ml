open! Core

module Fetcher = struct
  let prefix = "https://openlibrary.org/"

  module Subjects = struct
    let create_subject_search_url (subject : string) =
      prefix ^ "/subjects/" ^ subject ^ ".json"
    ;;

    let add_limit (curr_url : string) (limit : int) =
      curr_url ^ "?limit=" ^ Int.to_string limit
    ;;

    let fetch_sub ?limit (subject : string) =
      let start = create_subject_search_url subject in
      let url =
        match limit with None -> start | Some lim -> add_limit start lim
      in
      File_fetcher.fetch_exn Remote ~resource:url
    ;;
  end
end

let fetch_sub_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:""
    [%map_open
      let subject = flag "sub" (required string) ~doc:"the subject name" in
      fun () ->
        let fetched_file = Fetcher.Subjects.fetch_sub subject in
        print_endline fetched_file]
;;

let command =
  Command.group
    ~summary:"Fetch from Open Library"
    [ "subject-fetch", fetch_sub_command ]
;;
