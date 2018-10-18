function vertices(t: { left: { op: string, val: number }, op: string, right: { op: string, val: number } }|{ op: string, val: number }) {
//  console.log(JSON.stringify(t, null, 4));
//  console.log(t.op);
//  console.log(t["op"]);

  if (t.op === "node") {
    return 1 + vertices(t.left) +
               vertices(t.right);
  } else if (t.op === "leaf") {
    return 1;
  } else {
    console.log("error: "+ t["op"]);
    throw t.op;
  }
}

console.log(vertices({op: "node",
                      left: {op: "leaf", val: 1},
                      right: {op: "leaf", val: 2}}));
