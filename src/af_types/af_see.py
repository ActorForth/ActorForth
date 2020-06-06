# Introspection of words
from . import *
from continuation import Continuation

def see_handler(cont: AF_Continuation) -> None:
    print("Calling see_handler")
    symbol : str = cont.symbol.s_id
    t = Type.types.get(symbol)
    if t:
        so = StackObject("Type",Type(symbol))
        cont.stack.tos().value.push(so)
        return
    s : Stack = cont.stack.tos().value
    fcont = Continuation(s)
    cont.op, found = Type.op(cont.symbol.s_id, fcont)
    if found:
        for i in cont.op.words:
            print(i.name, i.sig)
    else:
        print("See: Failed to find word {}".format(cont.symbol.s_id))
        
    cont.stack.pop()
    
            
TSee = Type("See",see_handler)

def op_see(c: AF_Continuation) -> None:
    s = Stack()
    c.stack.push(StackObject(s,TSee))
Type.register_ctor('See', Operation('see',op_see),[])

