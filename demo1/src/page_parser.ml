open! Core

let parse_from_string (page : string) = Yojson.Safe.from_string page

module Subject_page = struct
  let make_book_from_json (book_info : Yojson.Safe.t) =
    match book_info with
    | `Assoc fields ->
      let key =
        List.find_map_exn fields ~f:(fun (name, key) ->
          if String.equal name "key" then Some key else None)
      in
      let title =
        List.find_map_exn fields ~f:(fun (name, title) ->
          if String.equal name "title" then Some title else None)
      in
      let isbn =
        List.find_map fields ~f:(fun (name, isbn) ->
          if String.equal name "isbn"
             && not (String.equal (Yojson.Safe.to_string isbn) "null")
          then Some isbn
          else None)
      in
      let subjects =
        List.find_map fields ~f:(fun (name, subject) ->
          if String.equal name "subject" then Some subject else None)
      in
      { Book.title = Yojson.to_string title
      ; key = Yojson.to_string key
      ; isbn =
          (match isbn with
           | None -> None
           | Some num -> Int.of_string (Yojson.to_string num))
      ; subjectsgit
      }
    | _ -> ()
  ;;

  let get_works_list (json_page : Yojson.Safe.t) =
    match json_page with
    | `Assoc top_level ->
      let _works =
        List.find top_level ~f:(fun (name, _elt) ->
          String.equal name "works")
      in
      ()
    | _ -> failwith "Not a valid formatted page"
  ;;
end
