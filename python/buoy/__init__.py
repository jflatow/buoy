"""
>>> b = Buoy()
>>> b['a'] = 1
>>> b['a']
1.0
>>> b.learn(['a', 'b'], 1)
0.0
>>> b.score(['a'])
1.0
>>> b = Buoy([('a', 1), ('b', 0)])
>>> b.score(['a', 'b'])
1.0
>>> k = b.keys()
>>> sorted(list(k))
['a', 'b']
>>> list(k)
[]
>>> len(b)
2
>>> l = Lexi()
>>> l.learn(('data', 1, 2), 5)
5.0
>>> l.score(('data', 1, 2))
5.0
"""
import os
import _buoy

class Buoy(_buoy.Buoy):
    def __init__(self, items=()):
        for attr, weight in items:
            self[attr] = weight

    def learn(self, attrs, score):
        delta = score - self.score(attrs)
        alpha = delta / len(attrs)
        for attr in attrs:
            self[attr] = self[attr] + alpha
        return delta

    def score(self, attrs):
        return sum(self[attr] for attr in attrs)

    def save(self):
        self.dump(open(self.path, 'w'))

    @classmethod
    def open(cls, path):
        self = cls.load(open(path)) if os.path.exists(path) else cls()
        self.path = path
        return self

class Lexi(Buoy):
    def learn(self, (doc, start, end), score):
        return _buoy.lexi_learn(self, doc, start, end, score)

    def score(self, (doc, start, end)):
        return _buoy.lexi_score(self, doc, start, end)
