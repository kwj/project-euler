
import unittest
from euler.bin.p0024 import compute

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_lst = list(range(10))
        test_patterns = [
            ((1_000_000, test_lst, len(test_lst)), '2783915460')
        ]

        for args, expected in test_patterns:
            with self.subTest(f'index: {args}'):
                self.assertEqual(expected, compute(*args))

if __name__ == '__main__':
    unittest.main()
