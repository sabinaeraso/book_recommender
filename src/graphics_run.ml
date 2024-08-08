open! Core

(* This is the core logic that actually runs the game. We have implemented
   all of this for you, but feel free to read this file as a reference. *)
let _every seconds ~f ~stop =
  let open Async in
  let rec loop () =
    if !stop
    then return ()
    else
      Clock.after (Time_float.Span.of_sec seconds)
      >>= fun () ->
      f ();
      loop ()
  in
  don't_wait_for (loop ())
;;

(*let handle_buttons (game : Game.t) ~game_over = every ~stop:game_over 0.001
  ~f:(fun () -> match Page_graphics.read_key () with | None -> () | Some 'd'
  -> Reader_input.restart game | Some key -> Reader_input.handle_key game
  key; Page_graphics.render game) ;; *)

let run () =
  Page_graphics.init_exn ();
  Page_graphics.render ()
;;

(*let game_over = ref false in *)
