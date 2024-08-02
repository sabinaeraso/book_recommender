open! Core

module Fetcher = struct
  let prefix = "https://openlibrary.org/"
  let suffix = ".json"

  let format_name (name : string) =
    String.strip name
    |> String.split_on_chars ~on:[ ' ' ]
    |> String.concat ~sep:"_"
    |> String.lowercase
  ;;

  module Subjects = struct
    let create_subject_search_url (subject : string) =
      prefix ^ "subjects/" ^ format_name subject ^ suffix
    ;;

    let add_limit (curr_url : string) (limit : int) =
      curr_url ^ "?limit=" ^ Int.to_string limit
    ;;

    let fetch_sub ?limit (subject : string) =
      let start = create_subject_search_url subject in
      let url =
        match limit with None -> start | Some lim -> add_limit start lim
      in
      let result = File_fetcher.fetch_exn Remote ~resource:url in
      (* print_endline result; *)
      result
    ;;
  end

  module Books = struct
    let create_book_search_url (key : string) = prefix ^ key ^ suffix

    let create_search_with_OLID (olid : string) =
      prefix ^ "works/" ^ olid ^ suffix
    ;;

    let fetch_key key =
      File_fetcher.fetch_exn Remote ~resource:(create_book_search_url key)
    ;;

    let fetch_olid olid =
      File_fetcher.fetch_exn Remote ~resource:(create_search_with_OLID olid)
    ;;
  end

  module Search_by_name = struct
    let create_search_url (name : string) =
      prefix ^ "search.json?q=" ^ format_name name
    ;;

    let fetch_from_search name =
      File_fetcher.fetch_exn Remote ~resource:(create_search_url name)
    ;;

    let create_edition_search_url (title : string) =
      prefix
      ^ "search.json/q="
      ^ format_name title
      ^ "&fields=key,title,author_name,editions,editions.key,editions.title,editions.ebook_access,editions.language"
    ;;

    let fetch_edition_language_page (title : string) =
      let url = create_edition_search_url title in
      let result = File_fetcher.fetch_exn Remote ~resource:url in
      result
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
    ~summary:"given an olid. in format 'OL....' will return book info"
    [%map_open
      let olid = flag "olid" (required string) ~doc:"the OLID of the book" in
      fun () ->
        let fetched_file = Fetcher.Books.fetch_olid olid in
        print_endline fetched_file]
;;

let search_book_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Given a book name, tries to find it"
    [%map_open
      let name = flag "name" (required string) ~doc:"The book name" in
      fun () ->
        let fetched_file = Fetcher.Search_by_name.fetch_from_search name in
        print_endline fetched_file]
;;

let fetch_language_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Given a title, Ex. 'Prisoner of Azkaban' will return the language of \
       the first edition of the book"
    [%map_open
      let title = flag "-title" (required string) ~doc:" the subject name" in
      fun () ->
        let fetched_file =
          Fetcher.Search_by_name.fetch_edition_language_page title
        in
        print_endline fetched_file]
;;

let command =
  Command.group
    ~summary:"Fetch from Open Library"
    [ "subject-fetch", fetch_sub_command
    ; "book-fetch-by-key", fetch_key_command
    ; "book-fetch-by-olid", fetch_olid_command
    ; "search-book-name", search_book_command
    ; "fetch-language", fetch_language_command
    ]
;;
