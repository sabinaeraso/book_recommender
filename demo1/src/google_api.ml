open! Core

module Fetcher = struct
  let prefix = "https://www.googleapis.com/books/v1/volumes"
  let get_book_volume_url (id : string) = prefix ^ "/" ^ id

  let format_name (name : string) =
    String.strip name
    |> String.split_on_chars ~on:[ ' ' ]
    |> String.concat ~sep:"%20"
    |> String.lowercase
  ;;

  let create_search_volumes_url (name : string) =
    prefix ^ "?q=" ^ format_name name
  ;;

  let fetch_book_by_id id =
    File_fetcher.fetch_exn Remote ~resource:(get_book_volume_url id)
  ;;

  let search_book_by_name name =
    File_fetcher.fetch_exn Remote ~resource:(create_search_volumes_url name)
  ;;

  let fetch_by_self_link link = File_fetcher.fetch_exn Remote ~resource:link
end

module Parser = struct
  let parse_from_string (page : string) = Yojson.Safe.from_string page

  let get_entry_type_from_search raw_page (entry : string) =
    let page_json = parse_from_string raw_page in
    match page_json with
    | `Assoc page ->
      let items = Page_parser.find_field "items" page in
      (match items with
       | `List item_list ->
         let item_dic = List.hd_exn item_list in
         (match item_dic with
          | `Assoc fields ->
            let id = Page_parser.find_field entry fields in
            Page_parser.format_field (Yojson.Safe.to_string id)
          | _ -> failwith "entries not formatted well in item entry")
       | _ -> failwith "items entries not formatted well")
    | _ -> failwith "not proper google api page"
  ;;

  let get_entry_from_volumeinfo_json raw_page (entry : string) =
    let page_json = parse_from_string raw_page in
    match page_json with
    | `Assoc page ->
      let items = Page_parser.find_field "items" page in
      (match items with
       | `List item_list ->
         let item_dic = List.hd_exn item_list in
         (match item_dic with
          | `Assoc fields ->
            let info = Page_parser.find_field "volumeInfo" fields in
            (match info with
             | `Assoc info_fields -> Page_parser.find_field entry info_fields
             | _ -> failwith "entries not formatted well in volume info")
          | _ -> failwith "entries not formatted well in item entry")
       | _ -> failwith "items entries not formatted well")
    | _ -> failwith "not proper google api page"
  ;;

  let get_book_id_from_search_json raw_page =
    get_entry_type_from_search raw_page "id"
  ;;

  let get_description_from_search_json raw_page =
    Page_parser.format_field
      (Yojson.Safe.to_string
         (get_entry_from_volumeinfo_json raw_page "description"))
  ;;

  let get_authors_from_search_json raw_page =
    match get_entry_from_volumeinfo_json raw_page "authors" with
    | `List author_list ->
      List.map author_list ~f:(fun author ->
        Page_parser.format_field (Yojson.Safe.to_string author))
      |> String.concat ~sep:", "
    | _ -> failwith "authors not properly formatted"
  ;;

  let get_self_link_from_search raw_page =
    get_entry_type_from_search raw_page "selfLink"
  ;;

  let get_image_from_book raw_page (image_size : string) =
    let page_json = parse_from_string raw_page in
    match page_json with
    | `Assoc page ->
      let items = Page_parser.find_field "volumeInfo" page in
      (match items with
       | `Assoc item_map ->
         let images_list = Page_parser.find_field "imageLinks" item_map in
         (match images_list with
          | `Assoc images ->
            let image_link = Page_parser.find_field image_size images in
            Page_parser.format_field (Yojson.Safe.to_string image_link)
          | _ -> failwith "items entries not formatted well")
       | _ -> failwith "items entries not formatted well")
    | _ -> failwith "not proper google api page"
  ;;
end

let find_book_by_name =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Given a book name, tries to find it using Google API"
    [%map_open
      let name = flag "name" (required string) ~doc:"The book name" in
      fun () ->
        let fetched_file =
          Parser.get_self_link_from_search (Fetcher.search_book_by_name name)
        in
        print_s [%sexp (fetched_file : string)]]
;;

let find_image_link =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Given a book name, tries to find cover image using Google API"
    [%map_open
      let name = flag "name" (required string) ~doc:"The book name" in
      fun () ->
        let fetched_file =
          Fetcher.fetch_by_self_link
            (Parser.get_self_link_from_search
               (Fetcher.search_book_by_name name))
        in
        print_s
          [%sexp (Parser.get_image_from_book fetched_file "small" : string)]]
;;

let find_book_description =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Given a book name, tries to find book description using Google API"
    [%map_open
      let name = flag "name" (required string) ~doc:"The book name" in
      fun () ->
        let fetched_file =
          Parser.get_description_from_search_json
            (Fetcher.search_book_by_name name)
        in
        print_s [%sexp (fetched_file : string)]]
;;

let find_book_authors =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Given a book name, tries to find all authors using Google API"
    [%map_open
      let name = flag "name" (required string) ~doc:"The book name" in
      fun () ->
        let fetched_file =
          Parser.get_authors_from_search_json
            (Fetcher.search_book_by_name name)
        in
        print_s [%sexp (fetched_file : string)]]
;;

let command =
  Command.group
    ~summary:"Allows for using the Google Api to get info"
    [ "book-by-name", find_book_by_name
    ; "cover-by-name", find_image_link
    ; "description-by-name", find_book_description
    ; "authors-by-name", find_book_authors
    ]
;;
