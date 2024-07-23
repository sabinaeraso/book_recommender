open! Core

type t =
  | Yes of Book.t
  | No of Book.t
  | Read_yes of Book.t
  | Read_no of Book.t
[@@deriving sexp_of]
