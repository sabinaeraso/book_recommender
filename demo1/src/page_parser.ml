open! Core

let parse_from_string (page : string) = Yojson.Safe.from_string page

module Subject_page = struct
  let make_subject_list_from_json (json_list : Yojson.Safe.t) =
    match json_list with
    | `List subjects -> List.map subjects ~f:Yojson.Safe.to_string
    | _ -> failwith "Subjects not properly formatted"
  ;;

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
        make_subject_list_from_json
          (List.find_map_exn fields ~f:(fun (name, subject) ->
             if String.equal name "subject" then Some subject else None))
      in
      Book.create
        ~title:(Yojson.Safe.to_string title)
        ~key:(Yojson.Safe.to_string key)
        ~isbn:
          (match isbn with
           | None -> None
           | Some num -> Some (Int.of_string (Yojson.Safe.to_string num)))
        ~subjects
    | _ -> failwith "Was not properly formatted"
  ;;

  let make_book_list (works_list : Yojson.Safe.t) =
    match works_list with
    | `List all_works -> List.map all_works ~f:make_book_from_json
    | _ -> failwith "Works not properly formatted"
  ;;

  let get_works_list (json_page : Yojson.Safe.t) =
    match json_page with
    | `Assoc top_level ->
      let works =
        List.find_map top_level ~f:(fun (name, work) ->
          if String.equal name "works" then Some work else None)
      in
      (match works with
       | None -> failwith "There was no works field"
       | Some work -> make_book_list work)
    | _ -> failwith "Not a valid formatted page"
  ;;

  let parse_books (raw_page : string) =
    get_works_list (parse_from_string raw_page)
  ;;
end

module Book_page = struct end
