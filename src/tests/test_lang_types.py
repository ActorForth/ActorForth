import unittest, pytest

from lang_types import is_constexpr, Nat, Literal 

class TestTypes( unittest.TestCase) :

    def test_Nat_isa(self) -> None:
        assert Nat.isa(0) is True
        assert Nat.isa(1) is True
        assert Nat.isa(-1) is False
        assert Nat.isa('a') is False
        assert Nat.isa(1.0) is False

    def test_Nat_ctor(self) -> None:
        n = Nat(0)
        assert n.value is 0
        with pytest.raises(ValueError) as x:
            n = Nat(-1)
        with pytest.raises(ValueError) as x:
            n = Nat("1")            


    def test_Literal_isa(self) -> None:
        assert Literal.isa(0) is True
        assert Literal.isa('a') is True
        assert Literal.isa(Nat) is False
        assert Literal.isa(Literal) is False

    def test_Literal_ctor(self) -> None:
        n = Literal(0)
        assert n.value is 0
        s = Literal("I'm a string!")        
        assert s.value is "I'm a string!"
        with pytest.raises(ValueError) as x:
            l = Literal(Nat)

class TestTypeFunctions( unittest.TestCase ) :

    def test_is_constexpr(self) -> None:
        constexpr = [Literal, Literal]
        non_constexpr = [Nat, Nat]
        mixed_expr = constexpr + non_constexpr

        assert is_constexpr(constexpr) is True
        assert is_constexpr(non_constexpr) is False
        assert is_constexpr(mixed_expr) is False
