class Node { 
    public left: Leaf|Node;
    public right: Leaf|Node;
    constructor(left, right) { 
        this.left = left; 
        this.right = right; 
    }

    nodes() {
      return 1 + this.left.nodes() + this.right.nodes();
    }
}

class Leaf { 
    public data: number;
    constructor(data) {
        this.data = data; 
    }

    nodes() {
      return 1;
    }
} 

console.log(new Node(new Node(new Leaf(1), new Leaf(2)), new Node(new Leaf(1), new Leaf(2))).nodes());
