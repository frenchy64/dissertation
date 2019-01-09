fun pair x y=fn z=> z x y;

let val x1=fn y=> pair y y
in let val x2=fn y=> x1(x1(y))
in let val x3=fn y=> x2(x2(y))
in let val x4=fn y=> x3(x3(y))
in let val x5=fn y=> x4(x4(y))
in x5(fn z=> z) end end end end end; 
