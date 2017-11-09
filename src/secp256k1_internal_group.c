#include <string.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <secp256k1_group.h>

CAMLprim value ml_secp256k1_ge_const (value r, value x, value y, value infinity) {
    secp256k1_ge *g = Caml_ba_data_val(r);
    memcpy(&g->x, Caml_ba_data_val(x), sizeof(secp256k1_fe));
    memcpy(&g->y, Caml_ba_data_val(y), sizeof(secp256k1_fe));
    g->infinity = Bool_val(infinity);
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_const (value r, value x, value y, value z, value infinity) {
    secp256k1_gej *g = Caml_ba_data_val(r);
    memcpy(&g->x, Caml_ba_data_val(x), sizeof(secp256k1_fe));
    memcpy(&g->y, Caml_ba_data_val(y), sizeof(secp256k1_fe));
    memcpy(&g->z, Caml_ba_data_val(z), sizeof(secp256k1_fe));
    g->infinity = Bool_val(infinity);
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_storage_const (value r, value x, value y) {
    secp256k1_ge_storage *g = Caml_ba_data_val(r);
    memcpy(&g->x, Caml_ba_data_val(x), sizeof(secp256k1_fe));
    memcpy(&g->y, Caml_ba_data_val(y), sizeof(secp256k1_fe));
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_set_xy(value r, value x, value y) {
    secp256k1_ge_set_xy(Caml_ba_data_val(r), Caml_ba_data_val(x), Caml_ba_data_val(y));
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_set_xquad(value r, value x) {
    return Val_bool(secp256k1_ge_set_xquad(Caml_ba_data_val(r), Caml_ba_data_val(x)));
}

CAMLprim value ml_secp256k1_ge_set_xo_var(value r, value x, value odd) {
    return Val_bool(secp256k1_ge_set_xo_var(Caml_ba_data_val(r), Caml_ba_data_val(x), Int_val(odd)));
}

CAMLprim value ml_secp256k1_ge_is_infinity(value a) {
    return Val_bool(secp256k1_ge_is_infinity(Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_ge_is_valid_var(value a) {
    return Val_bool(secp256k1_ge_is_valid_var(Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_ge_neg(value r, value a) {
    secp256k1_ge_neg(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_set_gej(value r, value a) {
    secp256k1_ge_set_gej(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

/* void secp256k1_ge_set_all_gej_var(secp256k1_ge *r, const secp256k1_gej *a, size_t len, const secp256k1_callback *cb); */
/* void secp256k1_ge_set_table_gej_var(secp256k1_ge *r, const secp256k1_gej *a, const secp256k1_fe *zr, size_t len); */
/* void secp256k1_ge_globalz_set_table_gej(size_t len, secp256k1_ge *r, secp256k1_fe *globalz, const secp256k1_gej *a, const secp256k1_fe *zr); */

CAMLprim value ml_secp256k1_gej_set_infinity(value r) {
    secp256k1_gej_set_infinity(Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_set_ge(value r, value a) {
    secp256k1_gej_set_ge(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_eq_x_var(value x, value a) {
    return Val_int(secp256k1_gej_eq_x_var(Caml_ba_data_val(x), Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_gej_neg(value r, value a) {
    secp256k1_gej_neg(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_is_infinity(value a) {
    return Val_bool(secp256k1_gej_is_infinity(Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_gej_has_quad_y_var(value a) {
    return Val_bool(secp256k1_gej_has_quad_y_var(Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_gej_double_nonzero(value r, value a, value rzr) {
    secp256k1_gej_double_nonzero(Caml_ba_data_val(r), Caml_ba_data_val(a), Is_block(rzr) ? Caml_ba_data_val(Field(rzr, 0)) : NULL);
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_double_var(value r, value a, value rzr) {
    secp256k1_gej_double_var(Caml_ba_data_val(r), Caml_ba_data_val(a), Is_block(rzr) ? Caml_ba_data_val(Field(rzr, 0)) : NULL);
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_add_var(value r, value a, value b, value rzr) {
    secp256k1_gej_add_var(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b), Is_block(rzr) ? Caml_ba_data_val(Field(rzr, 0)) : NULL);
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_add_ge(value r, value a, value b) {
    secp256k1_gej_add_ge(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b));
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_add_ge_var(value r, value a, value b, value rzr) {
    secp256k1_gej_add_ge_var(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b), Is_block(rzr) ? Caml_ba_data_val(Field(rzr, 0)) : NULL);
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_add_zinv_var(value r, value a, value b, value bzinv) {
    secp256k1_gej_add_ge_var(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b), Caml_ba_data_val(bzinv));
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_clear(value a) {
    secp256k1_gej_clear(Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_clear(value a) {
    secp256k1_ge_clear(Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_to_storage(value r, value a) {
    secp256k1_ge_to_storage(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_from_storage(value r, value a) {
    secp256k1_ge_from_storage(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_storage_cmov(value r, value a, value flag) {
    secp256k1_ge_storage_cmov(Caml_ba_data_val(r), Caml_ba_data_val(a), Bool_val(flag));
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_rescale(value r, value b) {
    secp256k1_gej_rescale(Caml_ba_data_val(r), Caml_ba_data_val(b));
    return Val_unit;
}
