open Async
open! Core

val every : float -> f:(unit -> unit) -> stop:bool -> unit
val get_user_response : unit -> Message.t Deferred.t
val run_recommender : Book_recommender.State.t -> unit Deferred.t
val run : unit -> unit Deferred.t
