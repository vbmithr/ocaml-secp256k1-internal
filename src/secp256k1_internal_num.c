#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <secp256k1_num.h>

CAMLprim value sizeof_secp256k1_num(value unit) {
    return Val_int(sizeof(secp256k1_num));
}

CAMLprim value ml_secp256k1_num_copy(value r, value a) {
    secp256k1_num_copy(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_get_bin(value r, value rlen, value a) {
    secp256k1_num_get_bin(Caml_ba_data_val(r), Int_val(rlen), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_set_bin(value r, value a, value alen) {
    secp256k1_num_set_bin(Caml_ba_data_val(r), Caml_ba_data_val(a), Int_val(alen));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_mod_inverse(value r, value a, value m) {
    secp256k1_num_mod_inverse(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(m));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_jacobi(value a, value b) {
    return Val_int(secp256k1_num_jacobi(Caml_ba_data_val(a), Caml_ba_data_val(b)));
}

CAMLprim value ml_secp256k1_num_cmp(value a, value b) {
    return Val_int(secp256k1_num_cmp(Caml_ba_data_val(a), Caml_ba_data_val(b)));
}

CAMLprim value ml_secp256k1_num_eq(value a, value b) {
    return Val_bool(secp256k1_num_eq(Caml_ba_data_val(a), Caml_ba_data_val(b)));
}

CAMLprim value ml_secp256k1_num_add(value r, value a, value b) {
    secp256k1_num_add(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_sub(value r, value a, value b) {
    secp256k1_num_sub(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_mul(value r, value a, value b) {
    secp256k1_num_mul(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_mod(value r, value m) {
    secp256k1_num_mod(Caml_ba_data_val(r), Caml_ba_data_val(m));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_shift(value r, value bits) {
    secp256k1_num_shift(Caml_ba_data_val(r), Int_val(bits));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_is_zero(value a) {
    return Val_bool(secp256k1_num_is_zero(Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_num_is_one(value a) {
    return Val_bool(secp256k1_num_is_one(Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_num_is_neg(value a) {
    return Val_bool(secp256k1_num_is_neg(Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_num_negate(value r) {
    secp256k1_num_negate(Caml_ba_data_val(r));
    return Val_unit;
}
