open! Core

let command =
  Command.group
    ~summary:"A tool for playing the wikipedia game, and other utilities"
    [ "fetch", Book_fetch.command ]
;;
