"""
repl.py - top level read/eval/print loop.

INTRO 1 : This is where ActorForth all begins for execution purposes.
"""
import sys

from continuation import Continuation, Stack
from interpret import interpret

from af_types.af_any import print_words

def print_stack_stats(stack):
    print("")
    print(stack.contents())
    print("Stack max_depth = %s" % stack.max_depth())
    print("Stack depth_history = %s" % stack.depth_history())
    print("Stack total operations = %s" % stack.total_operations())


if __name__ == "__main__":

    print("ActorForth demo interpreter. ^C to exit.")
    print_words()

    """
    INTRO 1.1 : Input always comes from a file whether that's the default
                stdin or a filename passed to the system.
    """
    handle = sys.stdin
    filename = "stdin"
    if len(sys.argv) >= 2:
  
        filename = sys.argv[1]
        handle = open(filename)

        print("Interpreting file: '%s'." % sys.argv[1])

    """
    INTRO 1.2 : Establish our stack and build our stateful Continutaion from it.
    """

    stack = Stack()
    cont = Continuation(stack)

    while True:

        """
        INTRO 1.3 : Continuously call the Interpreter until ^C is hit, the
                    input file runs out of tokens to parse, or an 
                    exception is encountered.

                    TODO: Likely probably want exceptions to just reset the 
                    stack/Continuation and resume.
        """

        try:
            cont = interpret(cont, handle, filename, prompt="ok: ")


            """
            INTRO 1.4 : If the last token in the input file is 'resume' 
                        then we re-establish stdin as our input file and
                        continue at the repl with everything intact. This
                        is a special hard-coded command.

                        TODO: How to do this in a more forth-like manner?

            INTRO 1.5 : Continue in interpret.py for INTRO stage 2.                        
            """
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
        except Exception as x:
            print( x )
            raise

    print_stack_stats(cont.stack)
    print("\nend of line...")

