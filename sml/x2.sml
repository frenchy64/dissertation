fun pair x y=fn z=> z x y;

let val x1=fn y=> pair y y
in let val x2=fn y=> x1(x1(y))
in x2(fn z=> z) end end; 
