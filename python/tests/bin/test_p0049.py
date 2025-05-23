import unittest

from euler.bin.p0049 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [((4,), '296962999629')]

        for args, expected in test_patterns:
            with self.subTest(f'limit: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
