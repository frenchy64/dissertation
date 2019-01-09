let fun pair x y=fn z=> z x y in
  let val x1=fn y=> pair y y in
    x1(fn z=> z)
  end
end;
