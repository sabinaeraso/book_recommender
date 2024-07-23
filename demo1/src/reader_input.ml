open! Core

let every (seconds : float) ~f ~stop =
  let open Async in
  let rec loop () =
    if stop
    then return ()
    else
      Clock.after (Time_float.Span.of_sec seconds)
      >>= fun () ->
      f ();
      loop ()
  in
  don't_wait_for (loop ())
;;

let response_options =
  Fzf.Pick_from.inputs
    [ "Interested"
    ; "Uninterested"
    ; "Already Read; Didn't Like"
    ; "Already Read; Did Like"
    ; "Done"
    ]
;;

let get_user_response = Fzf.pick_one response_options

(* let start_game = print_string "Please input the name of your favorite
   book! " in let origin_book = Page_parser.Book_page.parse_book
   (Book_fetch.Fetcher.Books.fetch_key "/works/OL82536W") in let state = *)
