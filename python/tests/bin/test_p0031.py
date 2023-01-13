
import unittest
from euler.bin.p0031 import compute

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            (([1, 2, 5, 10, 20, 50, 100, 200], 200), '73682')
        ]

        for args, expected in test_patterns:
            with self.subTest(f'coints: {args}'):
                self.assertEqual(expected, compute(*args))

if __name__ == '__main__':
    unittest.main()
