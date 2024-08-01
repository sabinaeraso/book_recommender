open Async
open! Core
open Fzf

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

let get_origin_book_input () =
  let%bind book_name =
    Async_interactive.ask_dispatch_gen
      ~f:(fun s -> Ok s)
      "What is the book you want to start with?  "
  in
  return book_name
;;

let rec get_origin_book () =
  let%bind name_to_search = get_origin_book_input () in
  let valid =
    Or_error.try_with (fun () ->
      Open_library.Fetch_and_parse.get_book_from_title name_to_search)
  in
  match valid with
  | Ok book -> return book
  | Error err ->
    print_endline (Error.to_string_hum err);
    print_endline
      "Could not find book of that title please try different title! \n";
    get_origin_book ()
;;

let get_valid_description (current_book : Book.t) =
  let valid_desc =
    Or_error.try_with (fun () ->
      Page_parser.Book_page.get_book_description
        (Book_fetch.Fetcher.Books.fetch_key current_book.ol_id))
  in
  let description =
    match valid_desc with
    | Ok desc -> desc
    | Error _ ->
      let google_desc =
        Or_error.try_with (fun () ->
          Google_api.Parser.get_description_from_search_json
            (Google_api.Fetcher.search_book_by_name current_book.title))
      in
      (match google_desc with
       | Ok desc -> "Google desc: " ^ desc
       | Error _ -> "No Description Found")
  in
  description
;;

let get_author_name (current_book : Book.t) =
  match current_book.author with
  | Some name -> name
  | None ->
    let google_authors =
      Or_error.try_with (fun () ->
        Google_api.Parser.get_authors_from_search_json
          (Google_api.Fetcher.search_book_by_name current_book.title))
    in
    (match google_authors with
     | Ok desc -> "Google author(s): " ^ desc
     | Error _ -> "No Author Found")
;;

let get_user_response (state : Book_recommender.State.t) =
  let current_book = state.current_book in
  let current_title = current_book.title in
  let description = get_valid_description current_book in
  let author_name = get_author_name current_book in
  let published =
    match current_book.publish_date with
    | None -> "No publish date"
    | Some date -> Int.to_string date
  in
  let open Deferred.Let_syntax in
  let%bind response =
    pick_one
      response_options
      ~prompt_at_top:()
      ~header:
        (current_title
         ^ "\n"
         ^ author_name
         ^ "\n"
         ^ Float.to_string current_book.heuristic
         ^ "\n"
         ^ description
         ^ "\n"
         ^ published
         ^ "\n")
  in
  match ok_exn response with
  | None -> failwith "Did not select one of the options"
  | Some message -> return message
;;

let handle_not_done message n ~state =
  match message with
  | Message.Interested -> Handle.handle_yes n ~state
  | Not_Interested -> Handle.handle_no ~state
  | Read_liked -> Handle.handle_read_yes n ~state
  | Read_didnt_like -> Handle.handle_no ~state
  | _ -> failwith "shouldnt have called this handle with that message "
;;

let rec run_recommender (n : float) (state : Book_recommender.State.t) =
  let%bind response = get_user_response state in
  match response with
  | Done -> return (Handle.handle_done ~state)
  | _ ->
    handle_not_done response n ~state;
    run_recommender (n +. 1.0) state
;;

let run () =
  print_endline "Please input the name of your favorite book: ";
  let%bind origin_book = get_origin_book () in
  let state = Book_recommender.State.empty_state origin_book in
  Handle.handle_read_yes 1.0 ~state;
  run_recommender 2.0 state
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
