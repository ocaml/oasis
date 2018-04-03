
open OUnit2
open OASISValues
open TestCommon

let tests =
  let expect_failure v s test_ctxt=
    try
      let ctxt = oasis_ctxt test_ctxt in
      let _ = v.parse ~ctxt s in
      assert_failure "expected an exception"
    with Failure _ ->
      ()
  in

  "OASISValues" >:::
  [
    "findlib_full_invalid_empty" >::
    expect_failure findlib_full "";

    "comma_separated_findlib_full_with_one_invalid_empty" >::
    expect_failure
      (comma_separated
         (with_optional_parentheses
            findlib_full
            OASISVersion.comparator_value))
      "pkg1, pkg2,";
  ]
