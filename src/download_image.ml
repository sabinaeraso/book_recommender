open! Core
open Async

(* let _download_image url filename : unit Deferred.t = let open
   Lwt.Let_syntax in let uri = Uri.of_string url in let lwt = let%bind
   _response, body = Cohttp_lwt_unix.Client.get uri in
   Cohttp_lwt.Body.to_string body in (* There exists a function which takes a
   callback whent his is determined / filled *) let contents = ref "" in
   Lwt.on_success lwt (fun l -> contents := l); Writer.save filename
   ~contents:!contents ;; *)

let download_image url (book_title : string) : unit Deferred.t =
  let raw_text = File_fetcher.fetch_exn Remote ~resource:url in
  let%bind.Deferred () =
    Writer.save ("images/" ^ book_title ^ ".jpeg") ~contents:raw_text
  in
  return ()
;;

(*Ivar*)
let command_download_image =
  let open Command.Let_syntax in
  Async_command.async
    ~summary:"Given a link write to a file cache"
    [%map_open
      let url = flag "url" (required string) ~doc:"url" in
      fun () ->
        (* let%bind.Deferred () = download_image url
           "images/felix_ever_after.txt" in let%bind.Deferred text =
           Reader.file_contents "images/felix_ever_after.txt" in
           print_endline text; Deferred.return () *)
        let%bind.Deferred () = download_image url "felix_ever_after" in
        let%bind.Deferred text =
          Reader.file_contents "images/felix_ever_after.jpeg"
        in
        print_endline text;
        Deferred.return ()]
;;
