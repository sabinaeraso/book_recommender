open! Core

let parse_from_string (page : string) = Yojson.Safe.from_string page

module Subject_page = struct
  let make_book_from_json (book_info : Yojson.Safe.t) =
    match book_info with `Assoc _fields -> () | _ -> ()
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
