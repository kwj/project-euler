
import unittest
from euler.bin.p0016 import compute

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((15,), '26'),
            ((1_000,), '1366')
        ]

        for args, expected in test_patterns:
            with self.subTest(f'exponent: {args}'):
                self.assertEqual(expected, compute(*args))

if __name__ == '__main__':
    unittest.main()
