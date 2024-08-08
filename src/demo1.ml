open! Core

let command =
  Command.group
    ~summary:
      "Allows you to fetch raw data, and parsed data from Open Library, run \
       program"
    [ "fetch", Book_fetch.command
    ; "parse", Page_parser.command
    ; "run", Reader_input.command
    ; "google-api", Google_api.command
    ; "cache", Cache.command
    ; "image", Download_image.command_download_image
    ]
;;
