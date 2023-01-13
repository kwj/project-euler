
import unittest
from euler.bin.p0084 import compute
from euler.lib.resource import asset_file

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((4, 100, 10_000), '101524')
        ]

        for args, expected in test_patterns:
            with self.subTest(f'faces, attempts, loop count: {args}'):
                self.assertEqual(expected, compute(*args))

if __name__ == '__main__':
    unittest.main()
