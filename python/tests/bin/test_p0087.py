
import unittest
from euler.bin.p0087 import compute

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((50,), '4'),
            ((50_000_000,), '1097343')
        ]

        for args, expected in test_patterns:
            with self.subTest(f'limit: {args}'):
                self.assertEqual(expected, compute(*args))

if __name__ == '__main__':
    unittest.main()
