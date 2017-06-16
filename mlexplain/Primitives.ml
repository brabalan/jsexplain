open Asttypes
open Btype
open Predef
open Types

let bin_op_type t =
  let f2_type = newgenty (Tarrow (Nolabel, t, t, Cok)) in (* t -> t *)
  newgenty (Tarrow (Nolabel, t, f2_type, Cok)) (* t -> t -> t *)

let bin_op_value t = {
  val_type = t ;
  val_kind = Val_reg ;
  val_loc = Location.none ;
  val_attributes = []
}

let int_bin_op =
  let t = bin_op_type type_int in
  bin_op_value t

let add_int_bin_ops env =
  let add_value env id = Env.add_value id int_bin_op env in
  let ops = ["+" ; "-" ; "*" ; "/" ; "mod"] in
  let ids = List.map Ident.create ops in
  List.fold_left add_value env ids
