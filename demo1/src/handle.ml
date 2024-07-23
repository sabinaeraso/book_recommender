open! Core

let _handle_yes ~(state : Book_recommender.State.t) ~(book : Book.t) =
  state.visited <- List.append state.visited [ book.key ];
  state.recommendations <- List.append state.recommendations [ book ];
  let subjects = book.subjects in
  List.iter subjects ~f:(fun subject ->
    Book_recommender.update_to_visit_from_subject ~state ~subject);
  let _next_book = Book_recommender.get_next_book ~state in
  (* give the user the next book title and description *)
  (* field for a new message after you return this next book to the user*)
  ()
;;

(* updates visited and recommendations with this book, updates to_visit with
   the books in all of the subjects, and gives the user the next book.*)

let _handle_no ~(state : Book_recommender.State.t) ~(book : Book.t) =
  state.visited <- List.append state.visited [ book.key ];
  let _next_book = Book_recommender.get_next_book ~state in
  (* give the user the next book title and description *)
  (* field for a new message after you return this next book to the user*)
  ()
;;

let _handle_read_yes ~(state : Book_recommender.State.t) ~(book : Book.t) =
  state.visited <- List.append state.visited [ book.key ];
  let subjects = book.subjects in
  List.iter subjects ~f:(fun subject ->
    Book_recommender.update_to_visit_from_subject ~state ~subject);
  let _next_book = Book_recommender.get_next_book ~state in
  ()
;;

let _handle_read_no ~(state : Book_recommender.State.t) ~(book : Book.t) =
  state.visited <- List.append state.visited [ book.key ];
  let _next_book = Book_recommender.get_next_book ~state in
  (* give the user the next book title and description *)
  (* field for a new message after you return this next book to the user*)
  ()
;;
