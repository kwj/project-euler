
import unittest
from euler.bin.p0011 import compute

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((), '70600674')
        ]

        for args, expected in test_patterns:
            with self.subTest('grid 20x20'):
                self.assertEqual(expected, compute(*args))

if __name__ == '__main__':
    unittest.main()
