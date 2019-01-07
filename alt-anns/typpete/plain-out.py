from typing import Dict, Sequence

def vertices(t: Dict[(Sequence, object)]) -> int:
    if (t['op'] == 'node'):
        return ((vertices(t['left']) + vertices(t['right'])) + 1)
    elif (t['op'] == 'leaf'):
        return 1
    else:
        raise Exception('no')
print(vertices({'op': 'node', 'left': {'op': 'leaf', 'val': 1}, 'right': {'op': 'leaf', 'val': 2}}))
