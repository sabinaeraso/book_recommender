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
  Fzf.Pick_from.assoc
    (List.zip_exn
       [ "Interested"
       ; "Uninterested"
       ; "Read and Liked"
       ; "Read did not like"
       ; "Done"
       ]
       Message.all)
;;

let get_user_response (state : Book_recommender.State.t) =
  let current_book = state.current_book in
  let current_title = current_book.title in
  let open Deferred.Let_syntax in
  let%bind response =
    pick_one response_options ~prompt_at_top:() ~header:current_title
  in
  match ok_exn response with
  | None -> failwith "Did not select one of the options"
  | Some message -> return message
;;

let rec run_recommender (state : Book_recommender.State.t) =
  let%bind response = get_user_response state in
  match response with
  | Message.Interested ->
    Handle.handle_yes ~state;
    run_recommender state
  | Not_Interested ->
    Handle.handle_no ~state;
    run_recommender state
  | Read_liked ->
    Handle.handle_read_yes ~state;
    run_recommender state
  | Read_didnt_like ->
    Handle.handle_read_no ~state;
    run_recommender state
  | Done -> return (Handle.handle_done ~state)
;;

let run () =
  print_endline "Please input the name of your favorite book! ";
  let origin_book =
    Page_parser.Book_page.parse_book
      (Book_fetch.Fetcher.Books.fetch_key "/works/OL82536W")
  in
  let state = Book_recommender.State.empty_state in
  state.current_book <- origin_book;
  Handle.handle_read_yes ~state;
  run_recommender state
;;

let run_command =
  let open Command.Let_syntax in
  Async_command.async
    ~summary:"Run the book recommender!"
    [%map_open
      let _subject =
        flag "subject" (optional string) ~doc:"the subject name"
      in
      fun () -> run ()]
;;

let command =
  Command.group
    ~summary:"Use to run the book recommender"
    [ "Recommend", run_command ]
;;
