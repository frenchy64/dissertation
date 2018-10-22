class Node { 
    public left;
    public right;
    constructor(left, right) { 
        this.left = left; 
        this.right = right; 
    }

    nodes() {
      return 1 + this.left.nodes() + this.right.nodes();
    }
}

class Leaf { 
    public data;
    constructor(data) {
        this.data = data; 
    }

    nodes() {
      return 1;
    }
} 

console.log(new Node(new Node(new Leaf(1), new Leaf(2)), new Leaf(2)).nodes());
