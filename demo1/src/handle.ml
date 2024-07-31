open! Core

let handle_yes distance_from_origin ~(state : Book_recommender.State.t) =
  let book = state.current_book in
  Book_recommender.update_visited ~state ~book;
  Book_recommender.update_recommendations ~state ~book;
  let subjects = book.subjects in
  (* this needs to change *)
  List.iter subjects ~f:(fun subject ->
    let valid_subject =
      Or_error.try_with (fun () ->
        Book_recommender.update_to_visit_from_subject
          distance_from_origin
          ~state
          ~subject)
    in
    match valid_subject with Ok _ -> () | Error _ -> ());
  let next_book = Book_recommender.get_next_book ~state in
  Book_recommender.update_current_book ~state ~new_book:next_book
;;

let handle_read_yes distance_from_origin ~(state : Book_recommender.State.t) =
  let book = state.current_book in
  Book_recommender.update_visited ~state ~book;
  let subjects = book.subjects in
  (* this needs to change *)
  List.iter subjects ~f:(fun subject ->
    let valid_subject =
      Or_error.try_with (fun () ->
        Book_recommender.update_to_visit_from_subject
          (distance_from_origin -. 0.5)
          ~state
          ~subject)
    in
    match valid_subject with Ok _ -> () | Error _ -> ());
  let next_book = Book_recommender.get_next_book ~state in
  Book_recommender.update_current_book ~state ~new_book:next_book
;;

(* sends the "distance from the origin" as 0.5 closer to the origin than in
   acutality. this in turn means that the books found from the subjects of
   this book will have a better (lower) heuristic, which is valid because we
   know the reader actually enjoys this book, not only is interested in the
   book.*)

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
