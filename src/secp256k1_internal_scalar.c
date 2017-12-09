#include <string.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>

#include "num_impl.h"
#include "scalar_impl.h"

CAMLprim value ml_secp256k1_scalar_const (value r,
                                          value d7, value d6, value d5, value d4,
                                          value d3, value d2, value d1, value d0) {
    secp256k1_scalar s = SECP256K1_SCALAR_CONST(Int64_val(d7), Int64_val(d6), Int64_val(d5), Int64_val(d4),
                                                Int64_val(d3), Int64_val(d2), Int64_val(d1), Int64_val(d0));
    memcpy(Caml_ba_data_val(r), &s, sizeof(secp256k1_scalar));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_const_bytecode (value * argv, int argn)
{
    return ml_secp256k1_scalar_const(argv[0], argv[1], argv[2], argv[3],
                                     argv[4], argv[5], argv[6], argv[7],
                                     argv[8]);
}

CAMLprim value ml_secp256k1_scalar_clear(value r) {
    secp256k1_scalar_clear(Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_get_bits(value a, value offset, value count) {
    return Val_int(secp256k1_scalar_get_bits(Caml_ba_data_val(a), Int_val(offset), Int_val(count)));
}

CAMLprim value ml_secp256k1_scalar_get_bits_var(value a, value offset, value count) {
    return Val_int(secp256k1_scalar_get_bits_var(Caml_ba_data_val(a), Int_val(offset), Int_val(count)));
}

CAMLprim value ml_secp256k1_scalar_set_b32(value r, value bin) {
    int overflow;
    secp256k1_scalar_set_b32(Caml_ba_data_val(r), Caml_ba_data_val(bin), &overflow);
    return Val_bool(overflow);
}

CAMLprim value ml_secp256k1_scalar_set_int(value r, value v) {
    secp256k1_scalar_set_int(Caml_ba_data_val(r), Int_val(v));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_get_b32(value bin, value a) {
    secp256k1_scalar_get_b32(Caml_ba_data_val(bin), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_add(value r, value a, value b) {
    return Val_int(secp256k1_scalar_add(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b)));
}

CAMLprim value ml_secp256k1_scalar_cadd_bit(value r, value bit, value flag) {
    secp256k1_scalar_cadd_bit(Caml_ba_data_val(r), Int_val(bit), Bool_val(flag));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_mul(value r, value a, value b) {
    secp256k1_scalar_mul(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_shr_int(value r, value n) {
    return Val_int(secp256k1_scalar_shr_int(Caml_ba_data_val(r), Int_val(n)));
}

CAMLprim value ml_secp256k1_scalar_sqr(value r, value a) {
    secp256k1_scalar_sqr(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_inverse(value r, value a) {
    secp256k1_scalar_inverse(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_inverse_var(value r, value a) {
    secp256k1_scalar_inverse_var(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_negate(value r, value a) {
    secp256k1_scalar_negate(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_is_zero(value r) {
    return Val_bool(secp256k1_scalar_is_zero(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_scalar_is_one(value r) {
    return Val_bool(secp256k1_scalar_is_one(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_scalar_is_even(value r) {
    return Val_bool(secp256k1_scalar_is_even(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_scalar_is_high(value r) {
    return Val_bool(secp256k1_scalar_is_high(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_scalar_cond_negate(value r, value flag) {
    int ret = secp256k1_scalar_cond_negate(Caml_ba_data_val(r), Bool_val(flag));
    return (ret == -1 ? Val_true : Val_false);
}

CAMLprim value ml_secp256k1_scalar_get_num(value r, value a) {
    secp256k1_scalar_get_num(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_order_get_num(value r) {
    secp256k1_scalar_order_get_num(Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_eq(value a, value b) {
    return Val_bool(secp256k1_scalar_eq(Caml_ba_data_val(a), Caml_ba_data_val(b)));
}

CAMLprim value ml_secp256k1_mul_shift_var(value r, value a, value b, value shift) {
    secp256k1_scalar_mul_shift_var(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b), Int_val(shift));
    return Val_unit;
}











