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
end

module Parser = struct
  let parse_from_string (page : string) = Yojson.Safe.from_string page

  let get_book_id_from_search_json raw_page =
    let page_json = parse_from_string raw_page in
    match page_json with
    | `Assoc page ->
      let items = Page_parser.find_field "items" page in
      (match items with
       | `Assoc item_list ->
         let id = Page_parser.find_field "id" item_list in
         Page_parser.format_field (Yojson.Safe.to_string id)
       | _ -> failwith "items entries not formatted well")
    | _ -> failwith "not proper google api page"
  ;;

  let get_self_link_from_search raw_page =
    let page_json = parse_from_string raw_page in
    match page_json with
    | `Assoc page ->
      let items = Page_parser.find_field "items" page in
      (match items with
       | `Assoc item_list ->
         let id = Page_parser.find_field "selfLink" item_list in
         Page_parser.format_field (Yojson.Safe.to_string id)
       | _ -> failwith "items entries not formatted well")
    | _ -> failwith "not proper google api page"
  ;;

  let get_image_from_book raw_page image_size =
    let page_json = parse_from_string raw_page in
    match page_json with
    | `Assoc page ->
      let items = Page_parser.find_field "VolumeInfo" page in
      (match items with
       | `Assoc item_list ->
         let images_list = Page_parser.find_field "imageLinks" item_list in
         (match images_list with
          | `Assoc images ->
            let image_link = Page_parser.find_field image_size images in
            Page_parser.format_field (Yojson.Safe.to_string image_link)
          | _ -> failwith "items entries not formatted well")
       | _ -> failwith "items entries not formatted well")
    | _ -> failwith "not proper google api page"
  ;;
end
