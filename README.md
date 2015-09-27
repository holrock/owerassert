# owerassert

test program for ocaml ppx.
filter modify AST `assert` more outputs.
depend on batteries(dump).

```sh
% opam install batteries
% make
% ./test                                                                                                                                                            1 ↵ ✭
assert = x 1
x -> 2
1 -> 1
Fatal error: exception File "test.ml", line 2, characters 2-8: Assertion failed
```

base code
```ocaml
let test1 x =
  assert (x = 1)
```

mod code
```ocaml
let test1 x =
  let ower_assert_gensym_1 = x
  and ower_assert_gensym_2 = 1 in
  if ower_assert_gensym_1 = ower_assert_gensym_2
  then ()
  else
    (Printf.eprintf "assert %s" "=";
     Printf.eprintf " %s" "x";
     Printf.eprintf " %s" "1";
     Printf.eprintf "\n";
     Printf.eprintf "%s -> %s\n" "x" (Batteries.dump ower_assert_gensym_1);
     Printf.eprintf "%s -> %s\n" "1" (Batteries.dump ower_assert_gensym_2);
     assert false)
```
