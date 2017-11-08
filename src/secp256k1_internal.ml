module Num = struct
  type t = Cstruct.buffer

  external copy :
    t -> t -> unit = "ml_secp256k1_num_copy" [@@noalloc]
  external get_bin :
    Cstruct.buffer -> int -> t -> unit = "ml_secp256k1_num_get_bin" [@@noalloc]
  external set_bin :
    t -> Cstruct.buffer -> int -> unit = "ml_secp256k1_num_set_bin" [@@noalloc]
  external mod_inverse :
    t -> t -> t -> unit = "ml_secp256k1_num_mod_inverse" [@@noalloc]
  external jacobi :
    t -> t -> int = "ml_secp256k1_num_jacobi" [@@noalloc]
  external compare :
    t -> t -> int = "ml_secp256k1_num_cmp" [@@noalloc]
  external equal :
    t -> t -> bool = "ml_secp256k1_num_eq" [@@noalloc]
  external add :
    t -> t -> t -> unit = "ml_secp256k1_num_add" [@@noalloc]
  external sub :
    t -> t -> t -> unit = "ml_secp256k1_num_sub" [@@noalloc]
  external mul :
    t -> t -> t -> unit = "ml_secp256k1_num_mul" [@@noalloc]
  external modulo :
    t -> t -> unit = "ml_secp256k1_num_mod" [@@noalloc]
  external shift :
    t -> int -> unit = "ml_secp256k1_num_shift" [@@noalloc]
  external is_zero :
    t -> bool = "ml_secp256k1_num_is_zero" [@@noalloc]
  external is_one :
    t -> bool = "ml_secp256k1_num_is_one" [@@noalloc]
  external is_neg :
    t -> bool = "ml_secp256k1_num_is_neg" [@@noalloc]
  external negate :
    t -> unit = "ml_secp256k1_num_negate" [@@noalloc]

  let get_bin cs =
    Cstruct.(get_bin (to_bigarray cs) (len cs))
  let set_bin r cs =
    Cstruct.(set_bin r (to_bigarray cs) (len cs))
end

module Scalar = struct
  type t = Cstruct.buffer

  let size = 32

  external const :
    t -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> unit =
    "ml_secp256k1_fe_const" "ml_secp256k1_fe_const_bytecode" [@@noalloc]

  let const ?(d7=0L) ?(d6=0L) ?(d5=0L) ?(d4=0L) ?(d3=0L) ?(d2=0L) ?(d1=0L) ?(d0=0L) () =
    let buf = Cstruct.create size in
    const buf.buffer d7 d6 d5 d4 d3 d2 d1 d0 ;
    buf.buffer

  external clear :
    t -> unit = "ml_secp256k1_scalar_clear" [@@noalloc]
  external get_bits :
    t -> int -> int -> int = "ml_secp256k1_scalar_get_bits" [@@noalloc]
  external get_bits_var :
    t -> int -> int -> int = "ml_secp256k1_scalar_get_bits_var" [@@noalloc]
  external set_b32 :
    t -> Cstruct.buffer -> bool = "ml_secp256k1_scalar_set_b32" [@@noalloc]
  external set_int :
    Cstruct.buffer -> int -> unit = "ml_secp256k1_scalar_set_int" [@@noalloc]
  external get_b32 :
    Cstruct.buffer -> t -> unit = "ml_secp256k1_scalar_get_b32" [@@noalloc]
  external add :
    t -> t -> t -> bool = "ml_secp256k1_scalar_add" [@@noalloc]
  external cadd_bit :
    t -> int -> bool -> unit = "ml_secp256k1_scalar_cadd_bit" [@@noalloc]
  external mul :
    t -> t -> t -> unit = "ml_secp256k1_scalar_mul" [@@noalloc]
  external shr_int :
    t -> int -> int = "ml_secp256k1_scalar_shr_int" [@@noalloc]
  external sqr :
    t -> t -> unit = "ml_secp256k1_scalar_sqr" [@@noalloc]
  external inverse :
    t -> t -> unit = "ml_secp256k1_scalar_inverse" [@@noalloc]
  external inverse_var :
    t -> t -> unit = "ml_secp256k1_scalar_inverse_var" [@@noalloc]
  external negate :
    t -> t -> unit = "ml_secp256k1_scalar_negate" [@@noalloc]
  external is_zero :
    t -> bool = "ml_secp256k1_scalar_is_zero" [@@noalloc]
  external is_one :
    t -> bool = "ml_secp256k1_scalar_is_one" [@@noalloc]
  external is_even :
    t -> bool = "ml_secp256k1_scalar_is_even" [@@noalloc]
  external is_high :
    t -> bool = "ml_secp256k1_scalar_is_high" [@@noalloc]
  external cond_negate :
    t -> bool -> bool = "ml_secp256k1_scalar_cond_negate" [@@noalloc]
  external get_num :
    Num.t -> t -> unit = "ml_secp256k1_scalar_get_num" [@@noalloc]
  external order_get_num :
    Num.t -> unit = "ml_secp256k1_scalar_order_get_num" [@@noalloc]
  external compare :
    t -> t -> int = "ml_secp256k1_scalar_eq" [@@noalloc]
  external mul_shift_var :
    t -> t -> t -> int -> unit = "ml_secp256k1_scalar_eq" [@@noalloc]

  let set_b32 t buf = set_b32 t (Cstruct.to_bigarray buf)
  let get_b32 buf t = get_b32 (Cstruct.to_bigarray buf) t
end

module Field = struct
  type t = Cstruct.buffer
  type storage = Cstruct.buffer

  let size = 40
  let storage_size = 32

  external const :
    t -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> unit =
    "ml_secp256k1_fe_const" "ml_secp256k1_fe_const_bytecode" [@@noalloc]

  external storage_const :
    t -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> unit =
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
    t -> unit = "ml_secp256k1_fe_normalize" [@@noalloc]
  external normalize_weak :
    t -> unit = "ml_secp256k1_fe_normalize_weak" [@@noalloc]
  external normalize_var :
    t -> unit = "ml_secp256k1_fe_normalize_var" [@@noalloc]
  external normalizes_to_zero :
    t -> bool = "ml_secp256k1_fe_normalizes_to_zero" [@@noalloc]
  external normalizes_to_zero_var :
    t -> bool = "ml_secp256k1_fe_normalizes_to_zero_var" [@@noalloc]
  external set_int :
    t -> int -> unit = "ml_secp256k1_fe_set_int" [@@noalloc]
  external clear :
    t -> unit = "ml_secp256k1_fe_clear" [@@noalloc]
  external is_zero :
    t -> bool = "ml_secp256k1_fe_is_zero" [@@noalloc]
  external is_odd :
    t -> bool = "ml_secp256k1_fe_is_odd" [@@noalloc]
  external equal :
    t -> t -> bool = "ml_secp256k1_fe_equal" [@@noalloc]
  external equal_var :
    t -> t -> bool = "ml_secp256k1_fe_equal_var" [@@noalloc]
  external cmp_var :
    t -> t -> int = "ml_secp256k1_fe_cmp_var" [@@noalloc]
  external set_b32 :
    t -> Cstruct.buffer -> bool = "ml_secp256k1_fe_set_b32" [@@noalloc]
  external get_b32 :
    Cstruct.buffer -> t -> unit = "ml_secp256k1_fe_get_b32" [@@noalloc]
  external negate :
    t -> t -> int -> unit = "ml_secp256k1_fe_negate" [@@noalloc]
  external mul_int :
    t -> int -> unit = "ml_secp256k1_fe_mul_int" [@@noalloc]
  external add :
    t -> t -> unit = "ml_secp256k1_fe_add" [@@noalloc]
  external mul :
    t -> t -> t -> unit = "ml_secp256k1_fe_mul" [@@noalloc]
  external sqr :
    t -> t -> unit = "ml_secp256k1_fe_sqr" [@@noalloc]
  external sqrt :
    t -> t -> int = "ml_secp256k1_fe_sqrt" [@@noalloc]
  external is_quad_var :
    t -> bool = "ml_secp256k1_fe_is_quad_var" [@@noalloc]
  external inv :
    t -> t -> unit = "ml_secp256k1_fe_inv" [@@noalloc]
  external inv_var :
    t -> t -> unit = "ml_secp256k1_fe_inv_var" [@@noalloc]
  external inv_all_var :
    t -> Cstruct.buffer -> int -> unit = "ml_secp256k1_fe_inv_all_var" [@@noalloc]
  external to_storage :
    storage -> t -> unit = "ml_secp256k1_fe_to_storage" [@@noalloc]
  external from_storage :
    t -> storage -> unit = "ml_secp256k1_fe_from_storage" [@@noalloc]
  external storage_cmov :
    storage -> storage -> bool -> unit = "ml_secp256k1_fe_storage_cmov" [@@noalloc]
  external cmov :
    t -> t -> bool -> unit = "ml_secp256k1_fe_cmov" [@@noalloc]

  let inv_all_var r fes =
    let nb_fe = List.length fes in
    let cs = Cstruct.create (nb_fe * size) in
    List.iteri
      (fun i fe -> Cstruct.(blit (of_bigarray fe) 0 cs (i*size) size)) fes ;
    inv_all_var r cs.buffer nb_fe ;
    Cstruct.memset cs 0

  let set_b32 t buf = set_b32 t (Cstruct.to_bigarray buf)
  let get_b32 buf t = get_b32 (Cstruct.to_bigarray buf) t

  let compare = cmp_var
end
