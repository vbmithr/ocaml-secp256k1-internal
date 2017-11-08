module Num : sig
  type t

  val copy : t -> t -> unit
  (** Copy a number. *)

  val get_bin : Cstruct.t -> t
  (** Convert a number's absolute value to a binary big-endian string.
      There must be enough place. *)

  val set_bin : t -> Cstruct.t -> unit
  (** Set a number to the value of a binary big-endian string. *)

  val mod_inverse : t -> t -> t -> unit
  (** Compute a modular inverse. The input must be less than the modulus. *)

  val jacobi : t -> t -> int
  (** Compute the jacobi symbol (a|b). b must be positive and odd. *)

  val compare : t -> t -> int
  (** Compare the absolute value of two numbers. *)

  val equal : t -> t -> bool
  (** Test whether two number are equal (including sign). *)

  val add : t -> t -> t -> unit
  (** Add two (signed) numbers. *)

  val sub : t -> t -> t -> unit
  (** Subtract two (signed) numbers. *)

  val mul : t -> t -> t -> unit
  (** Multiply two (signed) numbers. *)

  val modulo : t -> t -> unit
  (** Replace a number by its remainder modulo m. M's sign is
      ignored. The result is a number between 0 and m-1, even if r was
      negative. *)

  val shift : t -> int -> unit
  (** Right-shift the passed number by bits bits. *)

  val is_zero : t -> bool
  (** Check whether a number is zero. *)

  val is_one : t -> bool
  (** Check whether a number is one. *)

  val is_neg : t -> bool
  (** Check whether a number is strictly negative. *)

  val negate : t -> unit
  (** Change a number's sign. *)
end

(** Field element module.
 *
 *  Field elements can be represented in several ways, but code accessing
 *  it (and implementations) need to take certain properties into account:
 *  - Each field element can be normalized or not.
 *  - Each field element has a magnitude, which represents how far away
 *    its representation is away from normalization. Normalized elements
 *    always have a magnitude of 1, but a magnitude of 1 doesn't imply
 *    normality. *)
module Field : sig
  type t
  type storage

  val const :
    ?d7:int64 -> ?d6:int64 -> ?d5:int64 -> ?d4:int64 ->
    ?d3:int64 -> ?d2:int64 -> ?d1:int64 -> ?d0:int64 -> unit -> t
  (** Unpacks a constant into a overlapping multi-limbed FE
      element. *)

  val storage_const :
    ?d7:int64 -> ?d6:int64 -> ?d5:int64 -> ?d4:int64 ->
    ?d3:int64 -> ?d2:int64 -> ?d1:int64 -> ?d0:int64 -> unit -> t

  val normalize : t -> unit
  (** Normalize a field element. *)

  val normalize_weak : t -> unit
  (** Weakly normalize a field element: reduce it magnitude to 1, but
      don't fully normalize. *)

  val normalize_var : t -> unit
  (** Normalize a field element, without constant-time guarantee. *)

  val normalizes_to_zero : t -> bool
  (** Verify whether a field element represents zero i.e. would
      normalize to a zero value. The field implementation may
      optionally normalize the input, but this should not be relied
      upon. *)

  val normalizes_to_zero_var : t -> bool
  (** Verify whether a field element represents zero i.e. would
      normalize to a zero value. The field implementation may
      optionally normalize the input, but this should not be relied
      upon. *)

  val set_int : t -> int -> unit
  (** Set a field element equal to a small integer. Resulting field
      element is normalized. *)

  val clear : t -> unit
  (** Sets a field element equal to zero, initializing all fields. *)

  val is_zero : t -> bool
  (** Verify whether a field element is zero. Requires the input to be
      normalized. *)

  val is_odd : t -> bool
  (** Check the "oddness" of a field element. Requires the input to be
      normalized. *)

  val equal : t -> t -> bool
  (** Compare two field elements. Requires magnitude-1 inputs. *)

  val equal_var : t -> t -> bool
  (** Same as secp256k1_fe_equal, but may be variable time. *)

  val cmp_var : t -> t -> int
  (** Compare two field elements. Requires both inputs to be
      normalized. *)

  val compare : t -> t -> int
  (** Alias to [cmp_var]. *)

  val set_b32 : t -> int32 -> bool
  (** Set a field element equal to 32-byte big endian value. If
      successful, the resulting field element is normalized. *)

  val get_b32 : t -> int32
  (** Convert a field element to a 32-byte big endian value. Requires
      the input to be normalized. *)

  val negate : t -> t -> int -> unit
  (** Set a field element equal to the additive inverse of
      another. Takes a maximum magnitude of the input as an
      argument. The magnitude of the output is one higher. *)

  val mul_int : t -> int -> unit
  (** Multiplies the passed field element with a small integer
      constant. Multiplies the magnitude by that small integer. *)

  val add : t -> t -> unit
  (** Adds a field element to another. The result has the sum of the
      inputs' magnitudes as magnitude. *)

  val mul : t -> t -> t -> unit
  (** Sets a field element to be the product of two others. Requires
      the inputs' magnitudes to be at most 8.  The output magnitude is
      1 (but not guaranteed to be normalized). *)

  val sqr : t -> t -> unit
  (** Sets a field element to be the square of another. Requires the
      input's magnitude to be at most 8.  The output magnitude is 1
      (but not guaranteed to be normalized). *)

  val sqrt : t -> t -> int
  (** If a has a square root, it is computed in r and 1 is
      returned. If a does not have a square root, the root of its
      negation is computed and 0 is returned. The input's magnitude
      can be at most 8. The output magnitude is 1 (but not guaranteed
      to be normalized). The result in r will always be a square
      itself. *)

  val is_quad_var : t -> bool
  (** Checks whether a field element is a quadratic residue. *)

  val inv : t -> t -> unit
  (** Sets a field element to be the (modular) inverse of
      another. Requires the input's magnitude to be at most 8. The
      output magnitude is 1 (but not guaranteed to be normalized). *)

  val inv_var : t -> t -> unit
  (** Potentially faster version of secp256k1_fe_inv, without
      constant-time guarantee. *)

  val inv_all_var : t -> t list -> unit
  (** Calculate the (modular) inverses of a batch of field
      elements. Requires the inputs' magnitudes to be at most 8. The
      output magnitudes are 1 (but not guaranteed to be
      normalized). The inputs and outputs must not overlap in
      memory. *)

  val to_storage : t -> storage -> unit
  (** Convert a field element to the storage type. *)

  val from_storage : storage -> t -> unit
  (** Convert a field element back from the storage type. *)

  val storage_cmov : storage -> storage -> bool -> unit
  (** If flag is true, set *r equal to *a; otherwise leave
      it. Constant-time. *)

  val cmov : t -> t -> bool -> unit
  (** If flag is true, set *r equal to *a; otherwise leave
      it. Constant-time. *)
end
