import unittest

from lang_types import Nat 

class TestTypes( unittest.TestCase) :

    def test_Nat_isa(self) -> None:
        assert Nat.isa(0) is True
        assert Nat.isa(1) is True
        assert Nat.isa(-1) is False
        assert Nat.isa('a') is False
        assert Nat.isa(1.0) is False
