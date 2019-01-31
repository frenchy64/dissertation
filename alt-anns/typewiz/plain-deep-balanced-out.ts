function nodes(t: { left: { left: { op: string, val: number }, op: string, right: { op: string, val: number } }, op: string, right: { left: { op: string, val: number }, op: string, right: { op: string, val: number } } }|{ left: { op: string, val: number }, op: string, right: { op: string, val: number } }|{ op: string, val: number }) {
  switch t.op {
    case "node": 
      return 1 + nodes(t.left) + nodes(t.right);
    case "leaf": return 1;
    default: throw t.op;
  }
}
console.log(nodes({op: "node",
                   left: {op: "node",
                          left: {op: "leaf", val: 1},
                          right: {op: "leaf", val: 2}},
                   right: {op: "node",
                           left: {op: "leaf", val: 1},
                           right: {op: "leaf", val: 2}}}));
