from . import *
from .af_int import *
from .af_branch import *
from copy import copy

# op_nop from continuation.
make_word_context('nop', op_nop)


def op_print(c: AF_Continuation) -> None:
    op1 = c.stack.pop().value
    print("'%s'" % op1)
    if c.prompt:
        print(c.prompt,end='',flush=True)
make_word_context('print', op_print, [TAny])


def op_stack(c: AF_Continuation) -> None:
    if c.stack.depth() == 0:
        print("(data stack empty)")
    else:
        for n in reversed(c.stack.contents()):
            print('%s'%str(n))
    if c.prompt:
        print(c.prompt,end='',flush=True)
make_word_context('stack', op_stack)
make_word_context('.s', op_stack)


def op_stack_depth(c: AF_Continuation) -> None:
    c.stack.push(StackObject(value=c.stack.depth(), stype=TInt))
make_word_context('.d', op_stack_depth, [], [TInt])


# TODO : Words like reset can't work because there's no stack pattern
#        that can be declared for them.
# def op_reset(c: AF_Continuation) -> None:
#     op_stack_depth(c)
#     op_dup(c)
#     op_print(c)
#     op_start_countdown(c)
#     op_drop(c)
#     op_loop(c)
# # Not actually an honest stack picture since it clears everything off the stack.
# make_word_context('.reset', op_reset, [], [])


def print_words() -> None:
    _t_def = Type.types.get("Any",None)
    _ops : Op_list = []
    if not _t_def:
        _t_def = TypeDefinition(ops_list = [])
        _ops = _t_def.ops_list
    else:
        _ops = _t_def.ops_list
    print("Global Dictionary : %s" % list(set([op.short_name() for op in _ops])) )
    for type_name in Type.types.keys():
        if not Type.is_generic_name(type_name):
            _t_def = Type.types.get(type_name,None)
            if _t_def:
                _ops = _t_def.ops_list
                print("%s Dictionary : %s" % (type_name,list(set([op.short_name() for op in _ops]))) )


def op_words(c: AF_Continuation) -> None:                
    print_words()

    if c.prompt:
        print(c.prompt,end='',flush=True)  
make_word_context('words', op_words)


def op_print_types(c: AF_Continuation) -> None:
    print("\nTypes:")
    for type_name in Type.types.keys():

        _t_def : TypeDefinition = Type.types[type_name]

        _ops : Op_list = _t_def.ops_list
        _handle = _t_def.op_handler

        print("\t%s op_handler = %s" % (type_name, _handle))
    if c.prompt:
        print(c.prompt,end='',flush=True)  
make_word_context('types', op_print_types)                


#
#   Should dup, swap, drop and any other generic stack operators 
#   dynamically determine the actual stack types on the stack and
#   create dynamic type signatures based on what are found?
#
def op_dup(c: AF_Continuation) -> None:
    op1 = c.stack.tos()
    s = StackObject(value = copy(op1.value), stype=op1.stype)
    c.stack.push(s)
    #c.stack.push(op1) # This allowed value object instance variables to be tied to each other across StackObjects!
make_word_context('dup', op_dup, [t("Any")],[TAny, TAny])


def op_swap(c: AF_Continuation) -> None:
    op1 = c.stack.pop()
    op2 = c.stack.pop()
    c.stack.push(op1)
    c.stack.push(op2)
make_word_context('swap', op_swap, [t("_a"), t("_b")],[t("_b"), t("_a")])


def op_drop(c: AF_Continuation) -> None:
    op1 = c.stack.pop()
make_word_context('drop', op_drop, [TAny])


def op_2dup(c: AF_Continuation) -> None:
    op1 = c.stack.tos()
    s1 = StackObject(value = copy(op1.value), stype = op1.stype)
    op_swap(c)
    op2 = c.stack.tos()
    s2 = StackObject(value = copy(op2.value), stype = op2.stype)
    op_swap(c)
    c.stack.push(s2)
    c.stack.push(s1)
make_word_context('2dup', op_2dup, [t("_a"), t("_b")],[t("_a"), t("_b"), t("_a"), t("_b")])
