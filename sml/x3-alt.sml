let fun pair x y=fn z=> z x y in
  let val x1=fn y=> pair y y in
    let val x2=fn y=> x1(x1(y)) in
      let val x3=fn y=> x2(x2(y)) in
        x3(fn z=> z)
      end
    end
  end
end;
