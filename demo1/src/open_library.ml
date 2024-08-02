open! Core

module Fetch_and_parse = struct
  let get_books_from_subject (subject : string) =
    Book_fetch.Fetcher.Subjects.fetch_sub subject ~limit:500
    |> Page_parser.Subject_page.parse_books
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
end
