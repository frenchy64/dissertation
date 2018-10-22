def main():
    print(Node(Leaf(1), Leaf(2)).nodes())

class Node:
    def __init__(self, left, right):
        self.left = left
        self.right = right
    def nodes(self):
        return self.left.nodes() + self.right.nodes() + 1
class Leaf:
    def __init__(self, val):
        self.val = val
    def nodes(self):
        return 1
