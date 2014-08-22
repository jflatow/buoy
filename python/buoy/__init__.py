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
import random
import _buoy

class NoConvergence(Exception):
    def __init__(self, buoy, lessons):
        self.buoy = buoy
        self.lessons = lessons

class Buoy(_buoy.Buoy):
    def __init__(self, items=()):
        for attr, weight in items:
            self[attr] = weight

    def items(self):
        for attr in self:
            yield attr, self[attr]

    def values(self):
        for attr, weight in self.items():
            yield weight

    def learn(self, attrs, score):
        delta = score - self.score(attrs)
        alpha = delta / len(attrs)
        for attr in attrs:
            self[attr] = self[attr] + alpha
        return delta

    def score(self, attrs):
        return sum(self[attr] for attr in attrs)

    def train(self, lessons, tolerance=1e-6, maxiter=10000, maxdelta=1e300):
        lessons = list(lessons)
        for i in xrange(maxiter):
            deltas = sum(abs(self.learn(*l)) for l in lessons)
            if deltas < tolerance:
                return deltas
            if deltas > maxdelta:
                raise NoConvergence(buoy, lessons)
            random.shuffle(lessons)
        raise NoConvergence(self, lessons)

    def pathos(self, lessons):
        return sorted((-abs(self.learn(*l)), l) for l in lessons)

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
