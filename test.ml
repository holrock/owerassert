let test1 x =
  assert (x = 1)

let test2 s =
  assert ("hoge" = s)

let _ = test1 2
let _ = test2 "foo"
