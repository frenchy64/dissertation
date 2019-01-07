def main() -> None:
    print(Node(Leaf(1), Leaf(2)).vertices())

class Node():

    def __init__(self: 'Node', left: 'Leaf', right: 'Leaf') -> None:
        self.left: 'Leaf' = left
        self.right: 'Leaf' = right

    def vertices(self: 'Node') -> int:
        return ((self.left.vertices() + self.right.vertices()) + 1)

class Leaf():

    def __init__(self: 'Leaf', val: int) -> None:
        self.val: int = val

    def vertices(self: 'Leaf') -> int:
        return 1
