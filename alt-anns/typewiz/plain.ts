function vertices(t) {
  if (t.op === "node") {
    return 1 + vertices(t.left) + vertices(t.right);
  } else if (t.op === "leaf") {
    return 1;
  } else {
    throw t.op;
  }
}

console.log(vertices({op: "node",
                      left: {op: "leaf", val: 1},
                      right: {op: "leaf", val: 2}}));
