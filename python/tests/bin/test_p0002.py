
import unittest
from euler.bin.p0002 import compute

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((100,), '44'),
            ((4_000_000,), '4613732')
        ]

        for args, expected in test_patterns:
            with self.subTest(f'limit: {args}'):
                self.assertEqual(expected, compute(*args))

if __name__ == '__main__':
    unittest.main()
