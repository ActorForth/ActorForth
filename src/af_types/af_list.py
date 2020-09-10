from dataclasses import dataclass

@dataclass
class List:
    ltype: Type
    value: list

def op_list(c: AF_Continuation) -> None:
    pass

def op_append(c: AF_Continuation) -> None:
    pass