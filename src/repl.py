"""
repl.py - top level read/eval/print loop.

TODO: Unit testing is not catching this file presently!! FIX!

INTRO 1 : This is where ActorForth all begins for execution purposes.
"""
from typing import TextIO, Tuple
import traceback
import sys
from io import StringIO

from continuation import Continuation, Stack
from interpret import interpret

#from af_types.af_any import print_words
#from af_types.af_debug import op_debug, op_on, op_off
#from af_types.af_environment import op_checkpoint

""" 
INTRO 2.1 : All types get imported imported here.""
"""
from af_types.af_any import *
from af_types.af_int import *
from af_types.af_bool import *
from af_types.af_debug import *
from af_types.af_see import *
from af_types.af_branch import *
from af_types.af_environment import *
from compiler import *

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


def afc(code: str) -> TextIO:
    """
    Given a string of ActorForth code, returns a file stream 
    that can be executed by the interpreter.

    cont = interpret(cont, afc("1 int 2 int +"))
    """
    return StringIO(code)


"""
INTRO 1.2 : Establish our stack and build our stateful Continutaion from it.
"""

stack = Stack()
rstack = Stack()
cont = Continuation(stack, rstack)



def do_repl(filename: str, handle: TextIO):    
    global cont

    # Set Debug on or off initially.
    op_debug(cont)
    op_off(cont)
    #op_on(cont)

    # Checkpoint our initial setup.
    op_checkpoint(cont)

    print("ActorForth interpreter. ^C to exit.")
    print_words()

    

    while True:

        """
        INTRO 1.3 : Continuously call the Interpreter until ^C is hit, the
                    input file runs out of tokens to parse, or an 
                    exception is encountered.

                    TODO: Likely probably want exceptions to just reset the 
                    stack/Continuation and resume.
        """

        try:
            cont.execute(interpret(cont, handle, filename, prompt=cont.prompt))


            """
            INTRO 1.4 : If the last token in the input file is 'resume' 
                        then we re-establish stdin as our input file and
                        continue at the repl with everything intact. This
                        is a special hard-coded command.

                        TODO: How to do this in a more forth-like manner?                    
            """
            #if cont.stack.tos().value == "resume":
            if cont.symbol is not None and cont.symbol.s_id == "resume":
                handle = sys.stdin
                filename = "stdin"
                #cont.stack.pop()
                print_continuation_stats(cont)
            else:
                print("Clean?? exit! cont.symbol = %s." % cont.symbol)
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
            cont.log.error( "REPL EXCEPTION TYPE %s : %s" % (type(x),x) )
            cont.log.error( "TRACEBACK : %s" % traceback.format_exc() )
            print( "REPL EXCEPTION TYPE %s : %s" % (type(x),x) )
            print( "TRACEBACK : %s" % traceback.format_exc() )

            # Turn debug on automatically.
            op_debug(cont)
            op_on(cont) 

            print_continuation_stats(cont)
            filename, handle = setup_stream_for_interpreter(force_stdio = True)

    print_continuation_stats(cont)
    print("\nend of line...")
    
    tos = cont.stack.tos()
    if tos != Stack.Empty:
        return tos.value
    else:
        return None

if __name__ == "__main__":
    filename, handle = setup_stream_for_interpreter()
    do_repl(filename, handle)

