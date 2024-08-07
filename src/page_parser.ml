open! Core

let parse_from_string (page : string) = Yojson.Safe.from_string page

(* Controls the formal that fields get saved in the Book.t*)
let format_field (str : string) =
  String.split_on_chars str ~on:[ '\\'; '\"' ] |> String.concat
;;

let format_field_year (str : string) =
  String.split_on_chars str ~on:[ '\\'; '\"'; '/'; ',' ] |> String.concat
;;

let subject_is_valid ~subject =
  let banned_keywords =
    [ "translation"
    ; "interpretation"
    ; "language"
    ; "large type books"
    ; "general"
    ; "ficti"
    ; "read"
    ; "litera"
    ; "juvenile"
    ; "nyt"
    ; "bestseller"
    ; "award"
    ; "picks"
    ; "medal"
    ; "collection"
    ; "amerikanisches englisch"
    ; "new york times"
    ; "electronic book"
    ; "ebook"
    ; "classic"
    ; "er tong wen xue"
    ; "other"
    ; "pdf"
    ; "chapter book"
    ; "large print books"
    ; "lexile"
    ; "roman pour"
    ; "romans, nouvelles"
    ; "813/.54"
    ; "813.54"
    ; "hardcover"
    ; "autographed"
    ; "children's stories"
    ; "tong hua"
    ; "deutschland"
    ; "audiobook"
    ; "translat"
    ]
  in
  List.fold_until
    banned_keywords
    ~finish:(fun _b -> true)
    ~init:true
    ~f:(fun state word ->
      let lower_subject = String.lowercase subject in
      if String.is_substring ~substring:word lower_subject
         || String.equal word lower_subject
      then Stop false
      else Continue state)
;;

let make_subject_list_from_json (json_list : Yojson.Safe.t) =
  match json_list with
  | `List subjects ->
    List.filter_map subjects ~f:(fun x ->
      let subject = Yojson.Safe.to_string x in
      let formatted_subject = format_field subject in
      if subject_is_valid ~subject:formatted_subject
      then Some formatted_subject
      else None)
  | _ -> failwith "Subjects not properly formatted"
;;

let find_field (field_name : string) (fields : (string * Yojson.Safe.t) list)
  =
  match
    List.find_map fields ~f:(fun (name, key) ->
      if String.equal name field_name then Some key else None)
  with
  | Some elt -> elt
  | None -> failwith "No info found for this field"
;;

let find_field_option
  (field_name : string)
  (fields : (string * Yojson.Safe.t) list)
  =
  List.find_map fields ~f:(fun (name, key) ->
    if String.equal name field_name then Some key else None)
;;

let parse_author (authors : Yojson.Safe.t) : Yojson.Safe.t option =
  match authors with
  | `List author_list ->
    (match List.hd author_list with
     | Some author_map ->
       (match author_map with
        | `Assoc fields -> Some (find_field "name" fields)
        | _ -> failwith "Author field not association list")
     | None -> None)
  | _ -> failwith "Not properly formatted author field"
;;

let get_publish_year (fields : (string * Yojson.Safe.t) list) =
  match find_field_option "first_publish_year" fields with
  | None -> None
  | Some year ->
    let date =
      Or_error.try_with (fun () ->
        Int.of_string (format_field_year (Yojson.Safe.to_string year)))
    in
    (match date with Ok da -> Some da | Error _ -> None)
;;

let make_book_from_json (book_info : Yojson.Safe.t) : Book.t =
  match book_info with
  | `Assoc fields ->
    let key = find_field "key" fields in
    let title = find_field "title" fields in
    let author =
      List.find_map fields ~f:(fun (name, author) ->
        if String.equal name "authors"
        then (
          match parse_author author with
          | Some auth_json ->
            Some (format_field (Yojson.Safe.to_string auth_json))
          | None -> None)
        else None)
    in
    let publish_date = get_publish_year fields in
    let isbn =
      List.find_map fields ~f:(fun (name, isbn) ->
        if String.equal name "isbn"
           && not (String.equal (Yojson.Safe.to_string isbn) "null")
        then (
          match isbn with
          | `List isbns -> Some (List.hd_exn isbns)
          | _ -> Some isbn)
        else None)
    in
    (* let subjects = make_subject_list_from_json (find_field "subject"
       fields) in *)
    Book.create
      ~title:(format_field (Yojson.Safe.to_string title))
      ~author
      ~ol_id:(format_field (Yojson.Safe.to_string key))
      ~google_id:""
      ~isbn:
        (match isbn with
         | None -> None
         | Some num ->
           Some (Int.of_string (format_field (Yojson.Safe.to_string num))))
      ~subjects:[]
      ~publish_date
  | _ -> failwith "Was not properly formatted"
;;

module Subject_page = struct
  let make_book_list (works_list : Yojson.Safe.t) : Book.t list =
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

  let parse_books (raw_page : string) : Book.t list =
    get_works_list (parse_from_string raw_page)
  ;;

  let get_work_count (raw_page : string) : int =
    let json = parse_from_string raw_page in
    match json with
    | `Assoc fields ->
      (match find_field "work_count" fields with
       | `Int count -> count
       | _ -> failwith "work count was not an int")
    | _ -> failwith "not a proper formatted OL json"
  ;;
end

module Book_page = struct
  let make_book_from_book_json (book_info : Yojson.Safe.t) =
    match book_info with
    | `Assoc fields ->
      let key = find_field "key" fields in
      let title = find_field "title" fields in
      let author =
        List.find_map fields ~f:(fun (name, author) ->
          if String.equal name "authors"
          then (
            match parse_author author with
            | Some auth_json ->
              Some (format_field (Yojson.Safe.to_string auth_json))
            | None -> None)
          else None)
      in
      let publish_date = get_publish_year fields in
      let isbn =
        List.find_map fields ~f:(fun (name, isbn) ->
          if String.equal name "isbn"
             && not (String.equal (Yojson.Safe.to_string isbn) "null")
          then Some isbn
          else None)
      in
      let subjects =
        make_subject_list_from_json (find_field "subjects" fields)
      in
      Book.create
        ~title:(format_field (Yojson.Safe.to_string title))
        ~author
        ~ol_id:(format_field (Yojson.Safe.to_string key))
        ~google_id:""
        ~isbn:
          (match isbn with
           | None -> None
           | Some num ->
             Some (Int.of_string (format_field (Yojson.Safe.to_string num))))
        ~subjects
        ~publish_date
    | _ -> failwith "Was not properly formatted"
  ;;

  let parse_book (raw_page : string) =
    make_book_from_book_json (parse_from_string raw_page)
  ;;

  let parse_subjects_from_book (raw_page : string) =
    match parse_from_string raw_page with
    | `Assoc fields ->
      make_subject_list_from_json (find_field "subjects" fields)
    | _ -> failwith ""
  ;;

  let get_book_description (raw_page : string) =
    let parsed = parse_from_string raw_page in
    match parsed with
    | `Assoc fields ->
      let desc = find_field "description" fields in
      (match desc with
       | `Assoc desc_map ->
         format_field
           (match find_field "value" desc_map with
            | `String desc_string -> desc_string
            | _ -> failwith "not string")
       | `String desc_string -> format_field desc_string
       | _ -> failwith "Description in Open Library not properly formatted")
    | _ -> failwith "Was not a properly formatted JSON file"
  ;;
end

module Search_page = struct
  let get_most_relevant_book (search_page : Yojson.Safe.t) =
    match search_page with
    | `Assoc top_level ->
      let docs =
        List.find_map_exn top_level ~f:(fun (name, doc) ->
          if String.equal name "docs" then Some doc else None)
      in
      (match docs with
       | `List search_list ->
         let most_rel = List.hd search_list in
         (match most_rel with
          | None -> failwith "No books from that search!"
          | Some book -> make_book_from_json book)
       | _ -> failwith "Docs not formatted correctly")
    | _ -> failwith "Returned page not formatted correctly"
  ;;

  let parse_searches (raw_page : string) : Book.t =
    get_most_relevant_book (parse_from_string raw_page)
  ;;

  let get_first_language_from_json (edition_info : string) =
    match parse_from_string edition_info with
    | `Assoc fields ->
      let docs = find_field "docs" fields in
      (match docs with
       | `List docs_list ->
         let first_book = List.hd_exn docs_list in
         (match first_book with
          | `Assoc entry ->
            let editions = find_field "editions" entry in
            (match editions with
             | `Assoc e ->
               let inner_docs = find_field "docs" e in
               (match inner_docs with
                | `List inner_docs_list ->
                  let this_doc = List.hd_exn inner_docs_list in
                  (match this_doc with
                   | `Assoc book ->
                     let language = find_field "language" book in
                     (match language with
                      | `List language_list ->
                        let first_lang = List.hd_exn language_list in
                        let string_first_lang =
                          Yojson.Safe.to_string first_lang
                        in
                        let formatted_lang =
                          format_field string_first_lang
                        in
                        formatted_lang
                      | _ -> failwith "no language")
                   | _ -> failwith "no inner doc")
                | _ -> failwith "no edition")
             | _ -> failwith "")
          | _ -> failwith "no entry")
       | _ -> failwith "no inner docs")
    | _ -> failwith "No entries in first doc"
  ;;
end

let fetch_books_by_subject =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Given a subject, Ex. 'love' will return all books with that subject"
    [%map_open
      let subject =
        flag "subject" (required string) ~doc:"the subject name"
      in
      fun () ->
        let fetched_file = Book_fetch.Fetcher.Subjects.fetch_sub subject in
        print_s [%sexp (Subject_page.parse_books fetched_file : Book.t list)]]
;;

let find_book_by_name =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Given a book name, tries to find it"
    [%map_open
      let name = flag "name" (required string) ~doc:"The book name" in
      fun () ->
        let fetched_file =
          Book_fetch.Fetcher.Search_by_name.fetch_from_search name
        in
        print_s [%sexp (Search_page.parse_searches fetched_file : Book.t)]]
;;

let first_language_from_title =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Given a book title, tries to find it"
    [%map_open
      let title = flag "title" (required string) ~doc:"Tintenherz" in
      fun () ->
        let fetched_file =
          Book_fetch.Fetcher.Search_by_name.fetch_edition_language_page title
        in
        let first_language =
          Search_page.get_first_language_from_json fetched_file
        in
        print_endline first_language]
;;

let command =
  Command.group
    ~summary:"Parse Open Library Data"
    [ "books-from-subject", fetch_books_by_subject
    ; "book-from-name", find_book_by_name
    ; "first-language", first_language_from_title
    ]
;;
