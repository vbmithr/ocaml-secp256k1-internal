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

let () =
  Alcotest.run "secp256k1-internal" [
    "Num", Num.runtest ;
  ]
