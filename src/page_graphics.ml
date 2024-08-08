open! Core

module Constants = struct
  let scaling_factor = 1.
  let area_height = 600. *. scaling_factor |> Float.iround_down_exn
  let header_height = 75. *. scaling_factor |> Float.iround_down_exn
  let area_width = 675. *. scaling_factor |> Float.iround_down_exn
end

let only_one : bool ref = ref false

let init_exn () =
  let open Constants in
  (* Should raise if called twice *)
  if !only_one
  then failwith "Can only call init_exn once"
  else only_one := true;
  Graphics.open_graph
    (Printf.sprintf " %dx%d" (area_height + header_height) area_width)
;;

(* let display_cover = (* generates and adds the cover of current_book to the
   page by interfacing with the camp_images library*) ;; *)

let render () =
  Graphics.display_mode false;
  (*display_cover ();*)
  Graphics.display_mode true;
  Graphics.synchronize ()
;;
