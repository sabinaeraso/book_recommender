open! Core

module Fetcher = struct
  let prefix = "https://openlibrary.org/"
  let suffix = ".json"

  module Subjects = struct
    let create_subject_search_url (subject : string) =
      prefix ^ "/subjects/" ^ subject ^ suffix
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

  module Books = struct
    let create_book_search_url (key : string) = prefix ^ key ^ suffix

    let create_search_with_OLID (olid : string) =
      prefix ^ "/works/" ^ olid ^ suffix
    ;;

    let fetch_key key =
      File_fetcher.fetch_exn Remote ~resource:(create_book_search_url key)
    ;;

    let fetch_olid olid =
      File_fetcher.fetch_exn Remote ~resource:(create_search_with_OLID olid)
    ;;
  end
end

let fetch_sub_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Given a subject, Ex. 'love' will return all books with that subject"
    [%map_open
      let subject =
        flag "subject" (required string) ~doc:"the subject name"
      in
      fun () ->
        let fetched_file = Fetcher.Subjects.fetch_sub subject in
        print_endline fetched_file]
;;

let fetch_key_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Given a key in form /works/... will return the book info"
    [%map_open
      let key =
        flag "key" (required string) ~doc:"the key in its url format"
      in
      fun () ->
        let fetched_file = Fetcher.Books.fetch_key key in
        print_endline fetched_file]
;;

let fetch_olid_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"given an olid. in for 'OL....' will return book info"
    [%map_open
      let olid = flag "olid" (required string) ~doc:"the OLID of the book" in
      fun () ->
        let fetched_file = Fetcher.Books.fetch_olid olid in
        print_endline fetched_file]
;;

let command =
  Command.group
    ~summary:"Fetch from Open Library"
    [ "subject-fetch", fetch_sub_command
    ; "book-fetch-by-key", fetch_key_command
    ; "book-fetch-by-olid", fetch_olid_command
    ]
;;
