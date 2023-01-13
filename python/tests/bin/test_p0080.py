
import unittest
from euler.bin.p0080 import compute

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((2, 100), '475'),
            ((100, 100), '40886')
        ]

        for args, expected in test_patterns:
            with self.subTest(f'limit, digit: {args}'):
                self.assertEqual(expected, compute(*args))

if __name__ == '__main__':
    unittest.main()
