open! Core

type t =
  | Interested
  | Not_Interested
  | Read_liked
  | Read_didnt_like
  | Done
[@@deriving sexp_of]
