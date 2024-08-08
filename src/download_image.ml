open! Core
open Async

let download_image url filename ~callback : 'a Deferred.t =
  let open Lwt.Let_syntax in
  let uri = Uri.of_string url in
  let lwt =
    let%bind _response, body = Cohttp_lwt_unix.Client.get uri in
    Cohttp_lwt.Body.to_string body
  in
  (* There exists a function which takes a callback whent his is determined /
     filled *)
  Lwt.wakeup_later lwt (fun () ->
    Writer.save filename ~contents;
    callback)
;;
(*Ivar*)
