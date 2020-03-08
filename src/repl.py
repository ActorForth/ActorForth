import sys

from interpret import *

def print_stack_stats(stack):
    print("")
    print(stack.contents())
    print("Stack max_depth = %s" % stack.max_depth())
    print("Stack depth_history = %s" % stack.depth_history())
    print("Stack total operations = %s" % stack.total_operations())


if __name__ == "__main__":

    print("ActorForth demo interpreter. ^C to exit.")
    print("Global Dictionary : %s" % list(set([op[0] for op in Type.types["Any"]])) )
    for type in Type.types.keys():
        if type != "Any":
            ops = Type.types.get(type,[])
            if len(ops):
                print("%s Dictionary : %s" % (type,list(set([op[0] for op in ops]))) )

    handle = sys.stdin
    filename = "stdin"
    if len(sys.argv) >= 2:
  
        filename = sys.argv[1]
        handle = open(filename)

        print("Interpreting file: '%s'." % sys.argv[1])

    stack = Stack()
    cont = Continuation(stack)

    while True:

        try:
            cont = interpret(cont, handle, filename, prompt="ok: ")

            if cont.stack.tos().value == "resume":
                handle = sys.stdin
                filename = "stdin"
                cont.stack.pop()
                print_stack_stats(cont.stack)
            else:
                break
        except KeyboardInterrupt as x:
            print(" key interrupt.")
            break

    print_stack_stats(cont.stack)
    print("\nend of line...")

