open Async
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

let get_user_response () =
  let open Deferred.Let_syntax in
  let%map response = Fzf.pick_one response_options in
  match ok_exn response with
  | None -> failwith "Did not select one of the options"
  | Some message -> message
;;

let rec run_recommender (state : Book_recommender.State.t) =
  let%map response = get_user_response () in
  match response with
  | Message.Interested ->
    Handle.handle_yes ~state;
    run_recommender state |> ignore
  | Not_Interested ->
    Handle.handle_no ~state;
    run_recommender state |> ignore
  | Read_liked ->
    Handle.handle_read_yes ~state;
    run_recommender state |> ignore
  | Read_didnt_like ->
    Handle.handle_read_no ~state;
    run_recommender state |> ignore
  | Done -> Handle.handle_done ~state
;;

let run () =
  print_string "Please input the name of your favorite\n   book! ";
  let origin_book =
    Page_parser.Book_page.parse_book
      (Book_fetch.Fetcher.Books.fetch_key "/works/OL82536W")
  in
  let state = Book_recommender.State.empty_state in
  state.current_book <- origin_book;
  Handle.handle_yes ~state;
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
