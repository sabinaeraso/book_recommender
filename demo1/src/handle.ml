open! Core

let handle_yes ~(state : Book_recommender.State.t) =
  let book = state.current_book in
  state.visited_books <- List.append state.visited_books [ book.key ];
  state.recommendations <- List.append state.recommendations [ book ];
  let subjects = book.subjects in
  List.iter subjects ~f:(fun subject ->
    let valid_subject =
      Or_error.try_with (fun () ->
        Book_recommender.update_to_visit_from_subject ~state ~subject)
    in
    match valid_subject with Ok _ -> () | Error _ -> ());
  let next_book = Book_recommender.get_next_book ~state in
  state.current_book <- next_book
;;

let handle_no ~(state : Book_recommender.State.t) =
  let book = state.current_book in
  state.visited_books <- List.append state.visited_books [ book.key ];
  let next_book = Book_recommender.get_next_book ~state in
  state.current_book <- next_book
;;

let handle_read_yes ~(state : Book_recommender.State.t) =
  let book = state.current_book in
  state.visited_books <- List.append state.visited_books [ book.key ];
  let subjects = book.subjects in
  List.iter subjects ~f:(fun subject ->
    let valid_subject =
      Or_error.try_with (fun () ->
        Book_recommender.update_to_visit_from_subject ~state ~subject)
    in
    match valid_subject with Ok _ -> () | Error _ -> ());
  let next_book = Book_recommender.get_next_book ~state in
  state.current_book <- next_book
;;

let handle_read_no ~(state : Book_recommender.State.t) =
  let book = state.current_book in
  state.visited_books <- List.append state.visited_books [ book.key ];
  let next_book = Book_recommender.get_next_book ~state in
  state.current_book <- next_book
;;

let handle_done ~(state : Book_recommender.State.t) =
  let recs = state.recommendations in
  Core.print_endline "Recommendations:";
  List.iter recs ~f:(fun book -> Book.print book)
;;
