let pair x f = f x x;;

let f1 x = pair x in
  let f2 x = f1 (f1 x) in
  let f3 x = f2 (f2 x) in
  let f4 x = f3 (f3 x) in
  let f5 x = f4 (f4 x) in
  fun z -> f5 (fun x -> x) z;;
