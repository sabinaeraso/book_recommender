open Async
open! Core
open! Fzf

val run_recommender : float -> Book_recommender.State.t -> unit Deferred.t
val run : ?path:string -> unit -> unit Deferred.t
val command : Async.Command.t
