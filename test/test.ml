open Secp256k1_internal

let assert_equal a b = assert (a = b)

module Num = struct
  open Num
  let basic () =
    let z = zero in
    assert_equal true (is_zero z)

  let runtest =
    [ "basic", `Quick, basic ;
    ]
end

module Scalar = struct
  open Scalar
  let basic () =
    let z = zero () in
    assert_equal true (is_zero z) ;
    set_int z 1 ;
    assert_equal false (is_zero z) ;
    assert_equal false (is_even z) ;
    assert_equal true (is_one z)

  let runtest =
    [ "basic", `Quick, basic ;
    ]
end

let () =
  Alcotest.run "secp256k1-internal" [
    "Num", Num.runtest ;
    "Scalar", Scalar.runtest ;
  ]
