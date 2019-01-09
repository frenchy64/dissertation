let fun pair x y = fn z => z x y in
  let fun x1 y = pair y y in
    let fun x2 y = x1(x1(y)) in
      x2(fn z => z)
    end
  end
end;
