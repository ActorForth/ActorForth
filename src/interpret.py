"""
interpret.py - outer interpreter for ActorForth.

INTRO 2 : The interpreter parses the input stream and executes it in 
          the context of the Continuation.
"""
from typing import TextIO, Optional
import traceback

from continuation import Continuation 
from parser import Parser
from af_types import Symbol, Location

""" 
INTRO 2.1 : All types get imported imported here.""
"""
from af_types.af_any import *
from af_types.af_int import *
from af_types.af_bool import *
from af_types.af_debug import *
from af_types.af_see import *
from compiler import *

def interpret(cont: Continuation, input_stream: TextIO, filename: Optional[str] = None, prompt: Optional[str] = None) -> Continuation:    
    """
    INTRO 2.2 : Setup a parser for the input stream (passed from repl.py).
    """
    p = Parser()
    p.open_handle(input_stream, filename)

    interpret_mode = True

    if prompt: print(prompt,end='',flush=True)    

    """
    INTRO 2.3 : For each token in the input stream...
    """
    last_line = 0
    for s_id, linenum, column in p.tokens():        
        """
        INTRO 2.4 : Construct a symbol from the token and updates the
                   Continuation's symbol.
        """

        if prompt and linenum != last_line: 
            print(prompt,end='',flush=True)    
            last_line += 1

        cont.symbol = Symbol( s_id, Location(p.filename,linenum,column) ) 

        # Drop out to terminal input if we're asked to resume.
        if s_id == "resume": return cont

        #if p.filename != "stdin":
        #    print(s_id) # TODO - do we really want this echo? Probably not.

        try:
            """
            INTRO 2.5 : Call execute on the Continuation...
            """
            cont.execute()
            if cont.debug: print("%s" % cont)

            """
            INTRO 2.6:  ...until the end of tokens or an execution occurs
                        then it returns the Continuation.
            """
                
        except Exception as x:
            # repl.py does this exact thing so this is redundant for now.
            # print("Interpreter Exception Type: %s : %s" % (type(x),x) )
            # print( "TRACEBACK : %s" % traceback.format_exc() )
            # print("Interpreting symbol %s" % cont.symbol)
            # print(cont)

            raise


    return cont

    """
    INTRO 2.7 : Continue to continuation.py for INTRO stage 3.
    """
