open Async
open! Core
open Fzf

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

let get_user_response () =
  let open Option.Let_syntax in 
   let%bind response = (Fzf.pick_one response_options) in 
    match (ok_exn response) with 
    | 
  ;;

let rec run_recommender (state:Book_recommender.State.t) =
let response = get_user_response () in 
match response with 
|  -> 
;;

let run () =
  print_string "Please input the name of your favorite\n   book! "
in
let origin_book =
  Page_parser.Book_page.parse_book
    (Book_fetch.Fetcher.Books.fetch_key "/works/OL82536W")
in
let state = Book_recommender.State.empty_state in
state.current_book <- origin_book;
Handle._handle_yes ~state; run_recommender state
;;
