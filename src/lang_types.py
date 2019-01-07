#
#   types.py    -   Built in types for our language.
#

class Nat:

    #@static 
    def isa( value ) -> bool:
        return isinstance(value, int) and value >= 0

