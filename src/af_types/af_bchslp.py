from dataclasses import dataclass
import binascii
from typing import Optional, Iterator, Tuple, Any
from io import StringIO
from enum import Enum


OPS = {
    "OP_0": b"\x00",
    "OP_PUSHDATA1": B"\x4c",
    "OP_PUSHDATA2": b"\x4d",
    "OP_PUSHDATA4": b"\x4e",
    "OP_PUSH20": b"\x14",
    "OP_1NEGATE": b"\x4f",
    "OP_RESERVED": b"\x50",
    "OP_1": b"\x51",
    "OP_2": b"\x52",
    "OP_3": b"\x53",
    "OP_4": b"\x54",
    "OP_5": b"\x55",
    "OP_6": b"\x56",
    "OP_7": b"\x57",
    "OP_8": b"\x58",
    "OP_9": b"\x59",
    "OP_10": b"\x5a",
    "OP_11": b"\x5b",
    "OP_12": b"\x5c",
    "OP_13": b"\x5d",
    "OP_14": b"\x5e",
    "OP_15": b"\x5f",
    "OP_16": b"\x60",

    # control
    "OP_NOP": b"\x61",
    "OP_VER": b"\x62",
    "OP_IF": b"\x63",
    "OP_NOTIF": b"\x64",
    "OP_VERIF": b"\x65",
    "OP_VERNOTIF": b"\x66",
    "OP_ELSE": b"\x67",
    "OP_ENDIF": b"\x68",
    "OP_VERIFY": b"\x69",
    "OP_RETURN": b"\x6a",

    # stack ops
    "OP_TO_ALT_STACK": b"\x6b",
    "OP_FROM_ALT_STACK": b"\x6c",
    "OP_2DROP": b"\x6d",
    "OP_2DUP": b"\x6e",
    "OP_3DUP": b"\x6f",
    "OP_2OVER": b"\x70",
    "OP_2ROT": b"\x71",
    "OP_2SWAP": b"\x72",
    "OP_IFDUP": b"\x73",
    "OP_DEPTH": b"\x74",
    "OP_DROP": b"\x75",
    "OP_DUP": b"\x76",
    "OP_NIP": b"\x77",
    "OP_OVER": b"\x78",
    "OP_PICK": b"\x79",
    "OP_ROLL": b"\x7a",
    "OP_ROT": b"\x7b",
    "OP_SWAP": b"\x7c",
    "OP_TUCK": b"\x7d",

    # splice ops
    "OP_CAT": b"\x7e",
    "OP_SPLIT": b"\x7f",    # after monolith upgrade (May 2018)
    "OP_NUM2BIN": b"\x80",  # after monolith upgrade (May 2018)
    "OP_BIN2NUM": b"\x81",  # after monolith upgrade (May 2018)
    "OP_SIZE": b"\x82",

    # bit logic
    "OP_INVERT": b"\x83",
    "OP_AND": b"\x84",
    "OP_OR": b"\x85",
    "OP_XOR": b"\x86",
    "OP_EQUAL": b"\x87",
    "OP_EQUALVERIFY": b"\x88",
    "OP_RESERVED1": b"\x89",
    "OP_RESERVED2": b"\x8a",

    # numeric
    "OP_1ADD": b"\x8b",
    "OP_1SUB": b"\x8c",
    "OP_2MUL": b"\x8d",
    "OP_2DIV": b"\x8e",
    "OP_NEGATE": b"\x8f",
    "OP_ABS": b"\x90",
    "OP_NOT": b"\x91",
    "OP_0NOTEQUAL": b"\x92",

    "OP_ADD": b"\x93",
    "OP_SUB": b"\x94",
    "OP_MUL": b"\x95",
    "OP_DIV": b"\x96",
    "OP_MOD": b"\x97",
    "OP_LSHIFT": b"\x98",
    "OP_RSHIFT": b"\x99",

    "OP_BOOL_AND": b"\x9a",
    "OP_BOOL_OR": b"\x9b",
    "OP_NUMEQUAL": b"\x9c",
    "OP_NUMEQUALVERIFY": b"\x9d",
    "OP_NUMNOTEQUAL": b"\x9e",
    "OP_LESS_THAN": b"\x9f",
    "OP_GREATER_THAN": b"\xa0",
    "OP_LESS_THAN_OR_EQUAL": b"\xa1",
    "OP_GREATER_THAN_OR_EQUAL": b"\xa2",
    "OP_MIN": b"\xa3",
    "OP_MAX": b"\xa4",

    "OP_WITHIN": b"\xa5",

    # crypto
    "OP_RIPEMD160": b"\xa6",
    "OP_SHA1": b"\xa7",
    "OP_SHA256": b"\xa8",
    "OP_HASH160": b"\xa9",
    "OP_HASH256": b"\xaa",
    "OP_CODESEPARATOR": b"\xab",
    "OP_CHECKSIG": b"\xac",
    "OP_CHECKSIGVERIFY": b"\xad",
    "OP_CHECKMULTISIG": b"\xae",
    "OP_CHECKMULTISIGVERIFY": b"\xaf",

    "OP_NOP1": b"\xb0",
    "OP_CHECKLOCKTIMEVERIFY": b"\xb1",
    "OP_CHECKSEQUENCEVERIFY": b"\xb2",
    "OP_NOP4": b"\xb3",
    "OP_NOP5": b"\xb4",
    "OP_NOP6": b"\xb5",
    "OP_NOP7": b"\xb6",
    "OP_NOP8": b"\xb7",
    "OP_NOP9": b"\xb8",
    "OP_NOP10": b"\xb9",

    "OP_CHECKDATASIG": b"\xba",
    "OP_CHECKDATASIGVERIFY": b"\xbb",

    "OP_PREFIXBEGIN": b"\xf0",
    "OP_PREFIXEND": b"\xf7",

    "OP_SMALLINTEGER": b"\xfa",
    "OP_PUBKEYS": b"\xfb",
    "OP_PUBKEYHASH": b"\xfd",
    "OP_PUBKEY": b"\xfe",

    "OP_INVALID_OPCODE": b"\x0f",
}
op_keys = list(OPS.keys())
op_values = list(OPS.values())


class NFT1Type(Enum):
    NFT1_GROUP_MINTING = b"\x81"
    NFT1_CHILD = b"\x41"


# Given an unhexlify'd byte code, return the OP string code.
def decode(byte : str) -> Optional[str]:
    try:
        return op_keys[op_values.index(byte)]
    except ValueError:
        return None

def decode_push(byte : str) -> int:
    op = int.from_bytes(byte, byteorder = 'little')
    if op == 0: return 0
    if op == 76: return 1
    if op == 77: return 2
    if op == 78: return 4
    if op == 20: return 20
    if op >= 81 and op <= 96: return op - 80
    assert False, "raw:'%s' value: %s is not a valid PUSH OP." % (byte, op)

def read_bytes(f : StringIO, count : int) -> str:
    bytes = f.read(count*2)
    if bytes == '': raise StopIteration
    raw = binascii.unhexlify(bytes)
    return raw

def read_counted_string( f: StringIO ) -> str:
    byte = read_bytes(f,1)
    try:
        count = decode_push(byte)
    except AssertionError:
        count = int.from_bytes(byte, byteorder = 'little')
    return read_bytes(f,count)

def parse_slp_genesis( f: StringIO ) -> Iterator[Tuple[str,Any]]:

    # Ticker
    ticker = read_counted_string(f)
    yield("Ticker", ticker.decode('ascii'))

    # Token Name
    name = read_counted_string(f)
    yield("Token Name", name.decode('ascii'))

    # URI
    uri = read_counted_string(f)
    if uri == b'\x00' : uri = ''
    yield("URI", uri)

    # Document Hash
    hash = read_counted_string(f)
    if hash == b'\x00' : hash = ''
    yield("Document Hash", hash)    

    # Decimals
    decimals = int.from_bytes(read_counted_string(f), byteorder = 'little')
    yield("Decimals", decimals)

    # Unknown
    unknown = read_counted_string(f)
    yield("Unknown", unknown)

    # Coins
    coin_str = read_counted_string(f)
    coins = int.from_bytes(coin_str, byteorder = 'big')
    yield("Coins", coins)

def parse_slp_send(f : StringIO ) -> Iterator[Tuple[str,Any]]:
    parent_tx_id = read_counted_string(f)
    yield("Parent TXID", parent_tx_id)

    # Coins
    try:
        while(True):
            coin_str = read_counted_string(f)
            coins = int.from_bytes(coin_str, byteorder = 'big')
            yield("Coins", coins)
    except StopIteration:
        pass

def parse_slp_mint(f : StringIO ) -> Iterator[Tuple[str,Any]]:
    parent_tx_id = read_counted_string(f)
    yield("Parent TXID", parent_tx_id)

    unknown = read_counted_string(f)
    yield("Unknown", unknown)

    coin_str = read_counted_string(f)
    coins = int.from_bytes(coin_str, byteorder = 'big')
    yield("Coins", coins)


def parse_slp(f : StringIO) -> Iterator[Tuple[str,Any]]:

    # Confirm OP_RETURN
    raw = read_bytes(f,1)
    if_op_return = decode(raw)
    assert if_op_return == 'OP_RETURN', "'%s' Not an OP_RETURN Op Code." % if_op_return
    yield( (if_op_return, raw) )

    # Confirm SLP
    count = int.from_bytes(read_bytes(f,1), byteorder = 'little')
    if_slp = read_bytes(f,count)
    assert if_slp == b'SLP\x00', '"%s" Not an SLP Transaction.' % if_slp
    yield( ("Output Type", if_slp.decode('ascii')[0:3]))

    # Token Type
    token_type = read_counted_string(f)
    try:
        yield( "Token Type", NFT1Type(token_type))
    except ValueError:
        raise ValueError('"%s" Not a valid NFT1Type.' % token_type)

    # Transaction Type
    transaction_type = read_counted_string(f)
    result = ("Transaction Type", transaction_type.decode('ascii'))
    yield result

    TransTypes = { 
                    'GENESIS' : parse_slp_genesis,
                    'SEND' : parse_slp_send,
                    'MINT' : parse_slp_mint,
                 }
    trans = TransTypes.get(result[1])

    if trans:
        for x in trans(f): yield x
    else:
        raise Exception("Don't recognize SLP '%s' Transaction Type." % result[1])
    
    try:
        while True:
            extra = read_bytes(f,1)
            yield("Extra Byte", extra)        
    except StopIteration:
        pass


if __name__ == "__main__":

    tokens = {
        'btx_genesis' : "6a04534c500001810747454e45534953034146430f4163746f72466f72746820436f696e4c004c0001004c0008000000000000000a",
        'btx_nft_send' : "6a04534c500001810453454e4420025aab14154371b9c900afd8b629104d5c8b41f0bbf3146577cc26da09080e3f080000000000000001080000000000000001080000000000000001080000000000000001080000000000000001080000000000000001080000000000000001080000000000000001080000000000000001080000000000000001",
        'btx_child_genesis' : "6a04534c500001410747454e455349534c00076368696c6430314c004c0001004c00080000000000000001",
        'btx_child_send' : "6a04534c500001410453454e44208bedb11c0852aec2781cf76c2b928ead655281df019f80c47701319d50bad02c080000000000000001",
        'nick_genesis_uri' : "6a04534c500001810747454e45534953034254540d4269745472617368546f6b656e1b687474703a2f2f7777772e626974636f696e7375636b732e636f6d20c4cf9ce4bfa0685a0de7098422af0d214660d58398422af0d214660d5fffffff010001020800000000000f4240",
        'nick_child_genesis' : "6a04534c500001410747454e45534953035442310a547261736842616279311d687474703a2f2f7777772e7472617368746f6b656e626162792e636f6d200000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff01004c00080000000000000001",
        'nick_fan_out' : "6a04534c500001810453454e44205e3f0a1f6b42ad3b45e9cbc1320284223e299fa8b588aec67b725638d120ad3e0800000000000000010800000000000000010800000000000000010800000000000000010800000000000000010800000000000000010800000000000000010800000000000000010800000000000000010800000000000000010800000000000000010800000000000000010800000000000000010800000000000000010800000000000000010800000000000000010800000000000000010800000000000000010800000000000f422e",
        'nick_child_send' : "6a04534c500001410453454e4420dde01ec279e9007b25c882d171004e30edf82d80bc87189f344b953f2d96637f080000000000000001",
        'nick_mint_50k_more' : "6a04534c50000181044d494e54205e3f0a1f6b42ad3b45e9cbc1320284223e299fa8b588aec67b725638d120ad3e010208000000000000c350",
    }

    for x in tokens.keys():
        print("\nToken: %s" % x) 
        [print("\t%s:%s" % (i,j)) for i,j in parse_slp(StringIO(tokens[x]))]
