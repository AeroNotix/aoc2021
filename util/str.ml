module S = String

let fold_left f x a =
  let r = ref x in
  for i = 0 to S.length a - 1 do
    r := f !r (S.get a i)
  done;
  !r
