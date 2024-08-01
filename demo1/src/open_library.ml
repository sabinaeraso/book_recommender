open! Core

module Fetch_and_parse = struct
  let get_books_from_subject (subject : string) =
    Book_fetch.Fetcher.Subjects.fetch_sub subject
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
    Book_fetch.Fetcher.Books.fetch_key key
    |> Page_parser.Book_page.parse_subjects_from_book
  ;;
end
