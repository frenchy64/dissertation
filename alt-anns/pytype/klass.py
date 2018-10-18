def main():
    print(Node(Leaf(1), Leaf(2)).vertices())

class Node:
    def __init__(self, left, right):
        self.left = left
        self.right = right
    def vertices(self):
        return self.left.vertices() + self.right.vertices() + 1
class Leaf:
    def __init__(self, val):
        self.val = val
    def vertices(self):
        return 1
