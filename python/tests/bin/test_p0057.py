
import unittest
from euler.bin.p0057 import compute

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((8,), '1'),
            ((1_000,), '153')
        ]

        for args, expected in test_patterns:
            with self.subTest(f'nth: {args}'):
                self.assertEqual(expected, compute(*args))

if __name__ == '__main__':
    unittest.main()
