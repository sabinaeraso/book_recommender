open! Core

let handle_yes ~(state : Book_recommender.State.t) =
  let book = state.current_book in
  state.visited <- List.append state.visited [ book.key ];
  state.recommendations <- List.append state.recommendations [ book ];
  let subjects = book.subjects in
  List.iter subjects ~f:(fun subject ->
    Book_recommender.update_to_visit_from_subject ~state ~subject);
  let next_book = Book_recommender.get_next_book ~state in
  state.current_book <- next_book;
  Book.print next_book
;;

let handle_no ~(state : Book_recommender.State.t) =
  let book = state.current_book in
  state.visited <- List.append state.visited [ book.key ];
  let next_book = Book_recommender.get_next_book ~state in
  state.current_book <- next_book;
  Book.print next_book
;;

let handle_read_yes ~(state : Book_recommender.State.t) =
  let book = state.current_book in
  state.visited <- List.append state.visited [ book.key ];
  let subjects = book.subjects in
  List.iter subjects ~f:(fun subject ->
    Book_recommender.update_to_visit_from_subject ~state ~subject);
  let next_book = Book_recommender.get_next_book ~state in
  state.current_book <- next_book;
  Book.print next_book
;;

let handle_read_no ~(state : Book_recommender.State.t) =
  let book = state.current_book in
  state.visited <- List.append state.visited [ book.key ];
  let next_book = Book_recommender.get_next_book ~state in
  state.current_book <- next_book;
  Book.print next_book
;;

let handle_done ~(state : Book_recommender.State.t) =
  let recs = state.recommendations in
  List.iter recs ~f:(fun book -> Book.print book)
;;
