let fun pair x y = fn z=> z x y in
  let fun x1 y = pair y y in
    let fun x2 y = x1(x1(y)) in
      let fun x3 y= x2(x2(y)) in
        x1
      end
    end
  end
end;
