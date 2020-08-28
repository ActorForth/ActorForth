from . import *

"""

DmovR : _ -> r:_.

PCSave : -> r:PC;
	cont.

countdown : Int -> r:PC;
	DmovR PCSave

loop : Int r:PC -> r:PC | r:None;
	rswap
	rdup
	rdup PCRestore

10 countdown 
"""