def nodes(t):
    if t['op'] == 'node':
        return nodes(t['left']) + nodes(t['right']) + 1
    elif t['op'] == 'leaf':
        return 1
    else:
        raise Exception("no")

print(nodes({'op': 'node',
             'left': {'op': 'leaf', 'val': 1},
             'right': {'op': 'leaf', 'val': 2}}))
