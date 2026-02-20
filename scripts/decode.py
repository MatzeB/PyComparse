#!/usr/bin/env python3
import dis, marshal, struct, sys, time, types

def show_pyc_file(fp):
    magic = fp.read(4)
    zero = fp.read(4)
    moddate = fp.read(4)
    modtime = time.asctime(time.localtime(struct.unpack('I', moddate)[0]))
    print("magic %s" % (hex(int.from_bytes(magic, 'little'))))
    print("moddate %s (%s)" % (hex(int.from_bytes(moddate, 'little')), modtime))
    source_size = int.from_bytes(fp.read(4), 'little')
    print(f"source size {source_size}")
    code = marshal.load(fp)
    show_code(code)
     
def show_code(code, indent=''):
    print(f"{indent}code")
    indent += '   '
    print(f"{indent}argcount {code.co_argcount}")
    print(f"{indent}nlocals {code.co_nlocals}")
    print(f"{indent}stacksize {code.co_stacksize}")
    print(f"{indent}flags {code.co_flags:04x}")
    show_hex("code", code.co_code, indent=indent)
    dis.disassemble(code)
    print(f"{indent}consts")
    for const in code.co_consts:
        if type(const) == types.CodeType:
            show_code(const, indent+'   ')
        else:
            print(f"   {indent}{const!r}")
    print(f"{indent}names {code.co_names}")
    print(f"{indent}varnames {code.co_varnames}")
    print(f"{indent}freevars {code.co_freevars}")
    print(f"{indent}cellvars {code.co_cellvars}")
    print(f"{indent}filename {code.co_filename}")
    print(f"{indent}name {code.co_name}")
    print(f"{indent}firstlineno {code.co_firstlineno}")
    show_hex("lnotab", code.co_lnotab, indent=indent)
     
def show_hex(label, h, indent):
    h = hex(int.from_bytes(h, 'little'))
    if len(h) < 60:
        print(f"{indent}{label} {h}")
    else:
        print(f"{indent}{label}")
        for i in range(0, len(h), 60):
            print(f"{indent}   {h[i:i+60]}")

def _main() -> None:
    argv = sys.argv
    mode: str | None = None
    if "--exec" in argv:
        mode = "exec"
        argv.remove("--exec")
    if "--single" in argv:
        mode = "single"
        argv.remove("--single")
    if "--eval" in argv:
        mode = "eval"
        argv.remove("--eval")

    if len(argv) == 1:
        if mode is not None:
            code = compile(sys.stdin.read(), "<stdin>", mode, dont_inherit=True, optimize=-1)
            show_code(code)
        else:
            show_pyc_file(sys.stdin.buffer)
    else:
        filename = sys.argv[1]
        if filename.endswith(".py"):
            with open(filename, "rb") as fp:
                data = fp.read()
            if mode is None:
                mode = "exec"
            code = compile(data, filename, mode, dont_inherit=True, optimize=-1)
            show_code(code)
        else:
            with open(filename, "rb") as fp:
                show_pyc_file(fp)

if __name__ == "__main__":
    _main()
