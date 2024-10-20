import unittest

from euler.bin.p0061 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        # maximum polygon, numbers
        #  4, [5625, 2556]
        #  5, [8128, 2882, 8281]
        #  8, [1281, 8128, 2882, 8256, 5625, 2512]
        test_patterns = [((4,), '8181'), ((5,), '19291'), ((8,), '28684')]

        for args, expected in test_patterns:
            with self.subTest('NONE'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
