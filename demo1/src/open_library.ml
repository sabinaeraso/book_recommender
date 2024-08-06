open! Core
open Async

module Fetch_and_parse = struct
  let get_books_from_subject (cache : Cache.t) (subject : string) =
    let%bind cached_file = Cache.get_from_cache cache subject in
    let fetched =
      match cached_file with
      | Some file -> file
      | None -> Book_fetch.Fetcher.Subjects.fetch_sub ~limit:1000 subject
    in
    return (Page_parser.Subject_page.parse_books fetched)
  ;;

  let get_book_from_key (key : string) =
    Book_fetch.Fetcher.Books.fetch_key key
    |> Page_parser.Book_page.parse_book
  ;;

  let get_book_from_title (title : string) =
    Book_fetch.Fetcher.Search_by_name.fetch_from_search title
    |> Page_parser.Search_page.parse_searches
  ;;

  let get_subjects_from_key (key : string) =
    let raw_json = Book_fetch.Fetcher.Books.fetch_key key in
    let valid_desc =
      Or_error.try_with (fun () ->
        Page_parser.Book_page.parse_subjects_from_book raw_json)
    in
    match valid_desc with Ok subjects -> subjects | Error _ -> []
  ;;

  let get_first_language_from_title (title : string) =
    let valid_language =
      Or_error.try_with (fun () ->
        Book_fetch.Fetcher.Search_by_name.fetch_edition_language_page title
        |> Page_parser.Search_page.get_first_language_from_json)
    in
    match valid_language with Ok lang -> lang | Error _ -> "eng"
  ;;
end
