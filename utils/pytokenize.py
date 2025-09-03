#!/usr/bin/env python3
from tokenize import tokenize, ENCODING, COMMENT, NL, ENDMARKER, OP, NAME, NEWLINE, NUMBER, STRING, INDENT, DEDENT
import sys

def main():
    # Tokenize using python tokenize module; report in the same format as
    # scanner_test.
    filename = sys.argv[1]
    adjust_eof_line = False
    with open(filename, "rb") as fp:
        for token in tokenize(fp.readline):
            type, string, start, end, line = token
            if type in (COMMENT, NL, ENCODING):
                continue
            linenum = end[0]
            if type == NAME:
                t = f"`{string}`"
            elif type == OP:
                t = f"`{string}`"
            elif type == NUMBER:
                t = string
                if "." in string or "e" in string and not string.startswith("0x"):
                    t = f"{float(string):f}"
                else:
                    t = f"{int(string, 0):d}"
            elif type == NEWLINE:
                # tokenize increments line numbers when a file ends without
                # newline and a "pseudo" newline is produce. Arguably the
                # source location should not be incremented.
                if not line.endswith("\n"):
                    adjust_eof_line = True
                else:
                    # tokenize report end of "previous" line contrary
                    # to scanner reporting on "next" line...
                    linenum += 1
                t = "new line"
            elif type == ENDMARKER:
                if adjust_eof_line:
                    linenum -= 1
                t = "end of file"
            elif type == INDENT:
                t = "indent"
            elif type == DEDENT:
                if adjust_eof_line:
                    linenum -= 1
                t = "dedent"
            elif type == STRING:
                # TODO: tokenize only returns the input as-is so we have to
                # translate it into the actual string "value". Currently this
                # is too naive and doesn't deal with escape sequences,
                # prefixes, ...
                t = f"\"{string[1:-1]}\""
            else:
                t = f"<unknown>: {token}"
            print(f"{linenum}: {t}")

if __name__ == "__main__":
    main()
