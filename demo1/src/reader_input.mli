open Async
open! Core
open! Fzf

val every : float -> f:(unit -> unit) -> stop:bool -> unit
val run_recommender : float -> Book_recommender.State.t -> unit Deferred.t
val run : unit -> unit Deferred.t
val command : Async.Command.t
