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
    Cstruct.buffer -> unit = "ml_secp256k1_fe_normalize"
  external normalize_weak :
    Cstruct.buffer -> unit = "ml_secp256k1_fe_normalize_weak"
  external normalize_var :
    Cstruct.buffer -> unit = "ml_secp256k1_fe_normalize_var"
  external normalizes_to_zero :
    Cstruct.buffer -> bool = "ml_secp256k1_fe_normalizes_to_zero"
  external normalizes_to_zero_var :
    Cstruct.buffer -> bool = "ml_secp256k1_fe_normalizes_to_zero_var"
  external set_int :
    Cstruct.buffer -> int -> unit = "ml_secp256k1_fe_set_int"
  external clear :
    Cstruct.buffer -> unit = "ml_secp256k1_fe_clear"
  external is_zero :
    Cstruct.buffer -> bool = "ml_secp256k1_fe_is_zero"
  external is_odd :
    Cstruct.buffer -> bool = "ml_secp256k1_fe_is_odd"
  external equal :
    Cstruct.buffer -> Cstruct.buffer -> bool = "ml_secp256k1_fe_equal"
  external equal_var :
    Cstruct.buffer -> Cstruct.buffer -> bool = "ml_secp256k1_fe_equal_var"
  external cmp_var :
    Cstruct.buffer -> Cstruct.buffer -> int = "ml_secp256k1_fe_cmp_var"
  external set_b32 :
    Cstruct.buffer -> Cstruct.buffer -> bool = "ml_secp256k1_fe_set_b32"
  external get_b32 :
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_fe_get_b32"
  external negate :
    Cstruct.buffer -> Cstruct.buffer -> int -> unit = "ml_secp256k1_fe_negate"
  external mul_int :
    Cstruct.buffer -> int -> unit = "ml_secp256k1_fe_mul_int"
  external add :
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_fe_add"
  external mul :
    Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_fe_mul"
  external sqr :
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_fe_sqr"
  external sqrt :
    Cstruct.buffer -> Cstruct.buffer -> int = "ml_secp256k1_fe_sqrt"
  external is_quad_var :
    Cstruct.buffer -> bool = "ml_secp256k1_fe_is_quad_var"
  external inv :
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_fe_inv"
  external inv_var :
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_fe_inv_var"
  external inv_all_var :
    Cstruct.buffer -> Cstruct.buffer -> int -> unit = "ml_secp256k1_fe_inv_all_var"
  external to_storage :
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_fe_to_storage"
  external from_storage :
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secp256k1_fe_from_storage"
  external storage_cmov :
    Cstruct.buffer -> Cstruct.buffer -> bool -> unit = "ml_secp256k1_fe_storage_cmov"
  external cmov :
    Cstruct.buffer -> Cstruct.buffer -> bool -> unit = "ml_secp256k1_fe_cmov"

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
end
