open! Core

let command =
  Command.group
    ~summary:
      "Allows you to fetch raw data, and parsed data from Open Library"
    [ "fetch", Book_fetch.command; "parse", Page_parser.command ]
;;
