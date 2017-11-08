module Num = struct
  type t = Cstruct.buffer

  external copy :
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_num_copy"
  external get_bin :
    Cstruct.buffer -> int -> Cstruct.buffer = "ml_secp256k1_num_get_bin"
  external set_bin :
    Cstruct.buffer -> Cstruct.buffer -> int -> unit = "ml_secp256k1_num_set_bin"
  external mod_inverse :
    Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_num_mod_inverse"
  external jacobi :
    Cstruct.buffer -> Cstruct.buffer -> int = "ml_secp256k1_num_jacobi"
  external compare :
    Cstruct.buffer -> Cstruct.buffer -> int = "ml_secp256k1_num_cmp"
  external equal :
    Cstruct.buffer -> Cstruct.buffer -> bool = "ml_secp256k1_num_eq"
  external add :
    Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_num_add"
  external sub :
    Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_num_sub"
  external mul :
    Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_num_mul"
  external modulo :
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_num_mod"
  external shift :
    Cstruct.buffer -> int -> unit = "ml_secp256k1_num_shift"
  external is_zero :
    Cstruct.buffer -> bool = "ml_secp256k1_num_is_zero"
  external is_one :
    Cstruct.buffer -> bool = "ml_secp256k1_num_is_one"
  external is_neg :
    Cstruct.buffer -> bool = "ml_secp256k1_num_is_neg"
  external negate :
    Cstruct.buffer -> unit = "ml_secp256k1_num_negate"

  let get_bin cs =
    Cstruct.(get_bin (to_bigarray cs) (len cs))
  let set_bin r cs =
    Cstruct.(set_bin r (to_bigarray cs) (len cs))
end

module Field = struct
  type t = Cstruct.buffer
  type storage = Cstruct.buffer

  let size = 40
  let storage_size = 32

  external const :
    Cstruct.buffer -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> unit =
    "ml_secp256k1_fe_const" "ml_secp256k1_fe_const_bytecode" [@@noalloc]

  external storage_const :
    Cstruct.buffer -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> unit =
    "ml_secp256k1_fe_storage_const" "ml_secp256k1_fe_const_storage_bytecode" [@@noalloc]

  let const ?(d7=0L) ?(d6=0L) ?(d5=0L) ?(d4=0L) ?(d3=0L) ?(d2=0L) ?(d1=0L) ?(d0=0L) () =
    let buf = Cstruct.create size in
    const buf.buffer d7 d6 d5 d4 d3 d2 d1 d0 ;
    buf.buffer

  let storage_const ?(d7=0L) ?(d6=0L) ?(d5=0L) ?(d4=0L) ?(d3=0L) ?(d2=0L) ?(d1=0L) ?(d0=0L) () =
    let buf = Cstruct.create storage_size in
    storage_const buf.buffer d7 d6 d5 d4 d3 d2 d1 d0 ;
    buf.buffer

  external normalize :
    Cstruct.buffer -> unit = "ml_secp256k1_fe_normalize" [@@noalloc]
  external normalize_weak :
    Cstruct.buffer -> unit = "ml_secp256k1_fe_normalize_weak" [@@noalloc]
  external normalize_var :
    Cstruct.buffer -> unit = "ml_secp256k1_fe_normalize_var" [@@noalloc]
  external normalizes_to_zero :
    Cstruct.buffer -> bool = "ml_secp256k1_fe_normalizes_to_zero" [@@noalloc]
  external normalizes_to_zero_var :
    Cstruct.buffer -> bool = "ml_secp256k1_fe_normalizes_to_zero_var" [@@noalloc]
  external set_int :
    Cstruct.buffer -> int -> unit = "ml_secp256k1_fe_set_int" [@@noalloc]
  external clear :
    Cstruct.buffer -> unit = "ml_secp256k1_fe_clear" [@@noalloc]
  external is_zero :
    Cstruct.buffer -> bool = "ml_secp256k1_fe_is_zero" [@@noalloc]
  external is_odd :
    Cstruct.buffer -> bool = "ml_secp256k1_fe_is_odd" [@@noalloc]
  external equal :
    Cstruct.buffer -> Cstruct.buffer -> bool = "ml_secp256k1_fe_equal" [@@noalloc]
  external equal_var :
    Cstruct.buffer -> Cstruct.buffer -> bool = "ml_secp256k1_fe_equal_var" [@@noalloc]
  external cmp_var :
    Cstruct.buffer -> Cstruct.buffer -> int = "ml_secp256k1_fe_cmp_var" [@@noalloc]
  external set_b32 :
    Cstruct.buffer -> Cstruct.buffer -> bool = "ml_secp256k1_fe_set_b32" [@@noalloc]
  external get_b32 :
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_fe_get_b32" [@@noalloc]
  external negate :
    Cstruct.buffer -> Cstruct.buffer -> int -> unit = "ml_secp256k1_fe_negate" [@@noalloc]
  external mul_int :
    Cstruct.buffer -> int -> unit = "ml_secp256k1_fe_mul_int" [@@noalloc]
  external add :
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_fe_add" [@@noalloc]
  external mul :
    Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_fe_mul" [@@noalloc]
  external sqr :
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_fe_sqr" [@@noalloc]
  external sqrt :
    Cstruct.buffer -> Cstruct.buffer -> int = "ml_secp256k1_fe_sqrt" [@@noalloc]
  external is_quad_var :
    Cstruct.buffer -> bool = "ml_secp256k1_fe_is_quad_var" [@@noalloc]
  external inv :
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_fe_inv" [@@noalloc]
  external inv_var :
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_fe_inv_var" [@@noalloc]
  external inv_all_var :
    Cstruct.buffer -> Cstruct.buffer -> int -> unit = "ml_secp256k1_fe_inv_all_var" [@@noalloc]
  external to_storage :
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_fe_to_storage" [@@noalloc]
  external from_storage :
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_fe_from_storage" [@@noalloc]
  external storage_cmov :
    Cstruct.buffer -> Cstruct.buffer -> bool -> unit = "ml_secp256k1_fe_storage_cmov" [@@noalloc]
  external cmov :
    Cstruct.buffer -> Cstruct.buffer -> bool -> unit = "ml_secp256k1_fe_cmov" [@@noalloc]

  let set_b32 r a =
    let cs = Cstruct.create 4 in
    Cstruct.BE.set_uint32 cs 0 a ;
    set_b32 r cs.buffer

  let get_b32 a =
    let cs = Cstruct.create 4 in
    get_b32 cs.buffer a ;
    Cstruct.BE.get_uint32 cs 0

  let inv_all_var r fes =
    let nb_fe = List.length fes in
    let cs = Cstruct.create (nb_fe * size) in
    List.iteri
      (fun i fe -> Cstruct.(blit (of_bigarray fe) 0 cs (i*size) size)) fes ;
    inv_all_var r cs.buffer nb_fe ;
    Cstruct.memset cs 0

  let compare = cmp_var
end
