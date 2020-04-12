from . import *

# op_nop from continuation.
Type.add_op(Operation('nop', op_nop), TypeSignature([],[]))


def op_print(c: AF_Continuation) -> None:
    op1 = c.stack.pop().value
    print("'%s'" % op1)
Type.add_op(Operation('print', op_print), TypeSignature([TAny],[]))


def op_stack(c: AF_Continuation) -> None:
    if c.stack.depth() == 0:
        print("(stack empty)")
    else:
        for n in reversed(c.stack.contents()):
            print('%s'%str(n))

Type.add_op(Operation('stack', op_stack), TypeSignature([],[]))


def print_words() -> None:
    _t_def = Type.types.get("Any",None)
    _ops : Op_list = []
    if not _t_def:
        _t_def = TypeDefinition(ops_list = [])
        _ops = _t_def.ops_list
    else:
        _ops = _t_def.ops_list
    print("Global Dictionary : %s" % list(set([op[0].short_name() for op in _ops])) )
    for type_name in Type.types.keys():
        if type_name != "Any":
            _t_def = Type.types.get(type_name,None)
            if _t_def:
                _ops = _t_def.ops_list
                if len(_ops):
                    print("%s Dictionary : %s" % (type_name,list(set([op[0].short_name() for op in _ops]))) )

def op_words(c: AF_Continuation) -> None:                
    print_words()
Type.add_op(Operation('words', op_words), TypeSignature([],[]))



#
#   Should dup, swap, drop and any other generic stack operators 
#   dynamically determine the actual stack types on the stack and
#   create dynamic type signatures based on what are found?
#
def op_dup(c: AF_Continuation) -> None:
    op1 = c.stack.tos()
    c.stack.push(op1)
Type.add_op(Operation('dup', op_dup), TypeSignature([TAny],[TAny, TAny]))


def op_swap(c: AF_Continuation) -> None:
    op1 = c.stack.pop()
    op2 = c.stack.pop()
    c.stack.push(op1)
    c.stack.push(op2)
Type.add_op(Operation('swap', op_swap), TypeSignature([TAny, TAny],[TAny, TAny]))


def op_drop(c: AF_Continuation) -> None:
    op1 = c.stack.pop()
Type.add_op(Operation('drop', op_drop), TypeSignature([TAny],[]))


def op_2dup(c: AF_Continuation) -> None:
    op1 = c.stack.tos()
    op_swap(c)
    op2 = c.stack.tos()
    op_swap(c)
    c.stack.push(op2)
    c.stack.push(op1)
Type.add_op(Operation('2dup', op_2dup), TypeSignature([TAny, TAny],[TAny, TAny]))
