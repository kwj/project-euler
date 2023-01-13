
import unittest
from euler.bin.p0085 import compute

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((2_000_000,), '2772')
        ]

        for args, expected in test_patterns:
            with self.subTest(f'target: {args}'):
                self.assertEqual(expected, compute(*args))

if __name__ == '__main__':
    unittest.main()
