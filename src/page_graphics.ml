open! Core

(*
module Colors = struct
  let black_for_text = Graphics.rgb 000 000 000
  let green_for_interested = Graphics.rgb 000 255 000
  let purple_for_read_and_liked = Graphics.rgb 128 0 128
  let head_color = Graphics.rgb 100 100 125
  let red_for_not_interested = Graphics.rgb 255 000 000
  let gold_for_read_and_didnt_like = Graphics.rgb 255 223 0
  
  
  let button_color button = 

  match Button.color button with 
  | Red -> red_for_not_interested
  | Green -> green_for_interested
  | Gold -> gold_for_read_and_didnt_like
  | Purpe -> purple_for_read_and_liked
  ;;

end 
 *)

module Constants = struct
  let scaling_factor = 1.
  let area_height = 600. *. scaling_factor |> Float.iround_down_exn
  let header_height = 75. *. scaling_factor |> Float.iround_down_exn
  let area_width = 675. *. scaling_factor |> Float.iround_down_exn

end

(*
module Button = struct 

type t =
  | Interested 
  | Not_Interested
  | Read_liked 
  | Read_didnt_like 



end  *)

let only_one : bool ref = ref false

let init_exn () = 
let open Constants in
  (* Should raise if called twice *)
  if !only_one
  then failwith "Can only call init_exn once"
  else only_one := true;
  Graphics.open_graph
    (Printf.sprintf
       " %dx%d"
       (area_height + header_height)
       area_width);
;;
(*
let display_cover = 
(* generates and adds the cover of current_book to the page by interfacing with the camp_images library*)
;; *)

(*
let handle_decision =
(* gets the user's decision on the current book and feeds it to reader_input.*)
;;
*)


let render () = 
(* to be recalled ever time the user makes a decision. *)
Graphics.display_mode false;
  (*display_cover ();*)
  Graphics.display_mode true;
  Graphics.synchronize ()
;;
