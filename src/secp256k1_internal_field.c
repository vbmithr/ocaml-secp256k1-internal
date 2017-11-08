#include <string.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <secp256k1_field.h>

CAMLprim value ml_secp256k1_fe_const (value r,
                                      value d7, value d6, value d5, value d4,
                                      value d3, value d2, value d1, value d0) {
    secp256k1_fe fe = SECP256K1_FE_CONST(Int_val(d7), Int_val(d6), Int_val(d5), Int_val(d4),
                                         Int_val(d3), Int_val(d2), Int_val(d1), Int_val(d0));
    memcpy(Caml_ba_data_val(r), &fe, 40);
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_const_bytecode (value * argv, int argn)
{
    return ml_secp256k1_fe_const(argv[0], argv[1], argv[2], argv[3],
                                 argv[4], argv[5], argv[6], argv[7],
                                 argv[8]);
}

CAMLprim value ml_secp256k1_fe_storage_const (value r,
                                              value d7, value d6, value d5, value d4,
                                              value d3, value d2, value d1, value d0) {
    secp256k1_fe fe = SECP256K1_FE_STORAGE_CONST(Int_val(d7), Int_val(d6), Int_val(d5), Int_val(d4),
                                                 Int_val(d3), Int_val(d2), Int_val(d1), Int_val(d0));
    memcpy(Caml_ba_data_val(r), &fe, 32);
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_storage_const_bytecode (value * argv, int argn)
{
    return ml_secp256k1_fe_storage_const(argv[0], argv[1], argv[2], argv[3],
                                         argv[4], argv[5], argv[6], argv[7],
                                         argv[8]);
}

CAMLprim value ml_secp256k1_fe_normalize(value r) {
    secp256k1_fe_normalize(Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_normalize_weak(value r) {
    secp256k1_fe_normalize_weak(Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_normalize_var(value r) {
    secp256k1_fe_normalize_var(Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_normalizes_to_zero(value r) {
    return Val_bool(secp256k1_fe_normalizes_to_zero(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_fe_normalizes_to_zero_var(value r) {
    return Val_bool(secp256k1_fe_normalizes_to_zero_var(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_fe_set_int(value r, value a) {
    secp256k1_fe_set_int(Caml_ba_data_val(r), Int_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_clear(value r) {
    secp256k1_fe_clear(Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_is_zero(value r) {
    return Val_bool(secp256k1_fe_is_zero(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_is_odd(value r) {
    return Val_bool(secp256k1_fe_is_odd(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_fe_equal(value a, value b) {
    return Val_bool(secp256k1_fe_equal(Caml_ba_data_val(a), Caml_ba_data_val(b)));
}

CAMLprim value ml_secp256k1_fe_equal_var(value a, value b) {
    return Val_bool(secp256k1_fe_equal_var(Caml_ba_data_val(a), Caml_ba_data_val(b)));
}

CAMLprim value ml_secp256k1_fe_cmp_var(value a, value b) {
    return Val_int(secp256k1_fe_cmp_var(Caml_ba_data_val(a), Caml_ba_data_val(b)));
}

CAMLprim value ml_secp256k1_fe_set_b32(value r, value a) {
    return Val_bool(secp256k1_fe_set_b32(Caml_ba_data_val(r), Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_fe_get_b32(value a, value r) {
    secp256k1_fe_get_b32(Caml_ba_data_val(a), Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_negate(value r, value a, value m) {
    secp256k1_fe_negate(Caml_ba_data_val(r), Caml_ba_data_val(a), Int_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_mul_int(value r, value a) {
    secp256k1_fe_mul_int(Caml_ba_data_val(r), Int_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_add(value r, value a) {
    secp256k1_fe_add(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_mul(value r, value a, value b) {
    secp256k1_fe_mul(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_sqr(value r, value a) {
    secp256k1_fe_sqr(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_sqrt(value r, value a) {
    return Val_bool(secp256k1_fe_sqrt(Caml_ba_data_val(r), Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_is_quad_var(value r) {
    return Val_bool(secp256k1_fe_is_quad_var(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_fe_inv(value r, value a) {
    secp256k1_fe_inv(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_inv_var(value r, value a) {
    secp256k1_fe_inv_var(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_inv_all_var(value r, value a, value len) {
    secp256k1_fe_inv_all_var(Caml_ba_data_val(r), Caml_ba_data_val(a), Long_val(len));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_to_storage(value r, value a) {
    secp256k1_fe_to_storage(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_from_storage(value r, value a) {
    secp256k1_fe_from_storage(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_storage_cmov(value r, value a, value flag) {
    secp256k1_fe_storage_cmov(Caml_ba_data_val(r), Caml_ba_data_val(a), Bool_val(flag));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_cmov(value r, value a, value flag) {
    secp256k1_fe_cmov(Caml_ba_data_val(r), Caml_ba_data_val(a), Bool_val(flag));
    return Val_unit;
}
















