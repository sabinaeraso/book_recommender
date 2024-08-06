open! Core
open Async

(* let handle_yes_google_api distance_from_origin ~(state :
   Book_recommender.State.t) = let book = state.current_book in
   Book_recommender.update_visited ~state ~book;
   Book_recommender.update_recommendations ~state ~book; let book_page_raw =
   Google_api.Fetcher.fetch_book_by_id book.google_id in let subjects =
   Google_api.Parser.get_categories_from_book book_page_raw in List.iter
   subjects ~f:(fun subject -> let valid_subject = Or_error.try_with (fun ()
   -> Book_recommender.update_to_visit_from_subject distance_from_origin
   ~state ~subject) in match valid_subject with Ok _ -> () | Error _ -> ());
   let next_book = Book_recommender.get_next_book ~state in
   Book_recommender.update_current_book ~state ~new_book:next_book ;;

   let handle_read_yes_google_api distance_from_origin ~(state :
   Book_recommender.State.t) = let book = state.current_book in
   Book_recommender.update_visited ~state ~book; let book_page_raw =
   Google_api.Fetcher.fetch_book_by_id book.google_id in let subjects =
   Google_api.Parser.get_categories_from_book book_page_raw in List.iter
   subjects ~f:(fun subject -> let valid_subject = Or_error.try_with (fun ()
   -> Book_recommender.update_to_visit_from_subject (distance_from_origin -.
   0.5) ~state ~subject) in match valid_subject with Ok _ -> () | Error _ ->
   ()); let next_book = Book_recommender.get_next_book ~state in
   Book_recommender.update_current_book ~state ~new_book:next_book ;; *)

let handle_read_yes distance_from_origin ~(state : Book_recommender.State.t) =
  let book = state.current_book in
  Book_recommender.update_visited ~state ~book;
  let subjects =
    Open_library.Fetch_and_parse.get_subjects_from_key book.ol_id
  in
  let%bind () =
    Deferred.List.iter ~how:`Sequential subjects ~f:(fun subject ->
      let valid_subject =
        Or_error.try_with (fun () ->
          Book_recommender.update_to_visit_from_subject
            (distance_from_origin -. 0.5)
            ~state
            ~subject)
      in
      match valid_subject with
      | Ok deferred ->
        let%bind () = deferred in
        return ()
      | Error _ -> return ())
  in
  let next_book = Book_recommender.get_next_book ~state in
  return (Book_recommender.update_current_book ~state ~new_book:next_book)
;;

let handle_yes distance_from_origin ~(state : Book_recommender.State.t) =
  let book = state.current_book in
  Book_recommender.update_recommendations ~state ~book;
  handle_read_yes distance_from_origin ~state
;;

let handle_no ~(state : Book_recommender.State.t) =
  let book = state.current_book in
  Book_recommender.update_visited ~state ~book;
  let next_book = Book_recommender.get_next_book ~state in
  Book_recommender.update_current_book ~state ~new_book:next_book
;;

let handle_done ~(state : Book_recommender.State.t) =
  let recs = state.recommendations in
  Core.print_endline "Recommendations:";
  List.iter recs ~f:(fun book -> Book.print book)
;;
