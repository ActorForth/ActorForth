"""
repl.py - top level read/eval/print loop.

TODO: Unit testing is not catching this file presently!! FIX!

INTRO 1 : This is where ActorForth all begins for execution purposes.
"""
from typing import TextIO, Tuple
import traceback
import sys

from continuation import Continuation, Stack
from interpret import interpret

from af_types.af_any import print_words

def print_continuation_stats(cont : Continuation):
    print("")
    print(cont)
    print("Stack max_depth = %s" % cont.stack.max_depth())
    print("Stack depth_history = %s" % cont.stack.depth_history())
    print("Stack total operations = %s" % cont.stack.total_operations())

    """
    INTRO 1.1 : Input always comes from a file whether that's the default
                stdin or a filename passed to the system.
    """
def setup_stream_for_interpreter(force_stdio: bool = False) -> Tuple[str, TextIO]:
    handle = sys.stdin
    filename = sys.stdin.name
    if not force_stdio and len(sys.argv) >= 2:
        filename = sys.argv[1]
        handle = open(filename)

        print("Interpreting file: '%s'." % sys.argv[1])
    return filename, handle

if __name__ == "__main__":

    print("ActorForth demo interpreter. ^C to exit.")
    print_words()

    filename, handle = setup_stream_for_interpreter()

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
            """
            if cont.stack.tos().value == "resume":
                handle = sys.stdin
                filename = "stdin"
                cont.stack.pop()
                print_continuation_stats(cont)
            else:
                break
        except KeyboardInterrupt as x:
            print(" key interrupt.")
            break
        except Exception as x:
            """
            INTRO 1.5 : Uncaught exceptions will interupt the interpreter, print status output, 
                        turn Debug on, reset the input to stdin and proceed again.

            
            INTRO 1.6 : Continue in interpret.py for INTRO stage 2.
            """
            print( "REPL EXCEPTION TYPE %s : %s" % (type(x),x) )
            print( "TRACEBACK : %s" % traceback.format_exc() )
            cont.debug = True
            print_continuation_stats(cont)
            filename, handle = setup_stream_for_interpreter(force_stdio = True)

    print_continuation_stats(cont)
    print("\nend of line...")

