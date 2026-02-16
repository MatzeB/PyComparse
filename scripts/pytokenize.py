#!/usr/bin/env python3
import ast
from tokenize import tokenize, ENCODING, COMMENT, NL, ENDMARKER, OP, NAME, NEWLINE, NUMBER, STRING, INDENT, DEDENT, ERRORTOKEN
import sys
import unicodedata

INT64_MAX = (1 << 63) - 1


def _render_bytes_for_scanner(data):
    out = []
    for b in data:
        if 0x20 <= b <= 0x7E:
            out.append(chr(b))
        else:
            out.append(f"\\x{b:02x}")
    return "".join(out)


def _format_string_token(raw):
    try:
        value = ast.literal_eval(raw)
        if isinstance(value, bytes):
            rendered = _render_bytes_for_scanner(value)
            return f"b\"{rendered}\""
        if isinstance(value, str):
            rendered = _render_bytes_for_scanner(value.encode("utf-8"))
            return f"\"{rendered}\""
    except Exception:
        pass

    quote_pos = -1
    for i, ch in enumerate(raw):
        if ch in ("'", '"'):
            quote_pos = i
            break
    if quote_pos < 0:
        return f"\"{raw}\""

    prefix = raw[:quote_pos].lower()
    literal = raw[quote_pos:]

    if literal.startswith("'''") or literal.startswith('"""'):
        value = literal[3:-3]
    else:
        value = literal[1:-1]

    if "b" in prefix and "f" not in prefix:
        return f"b\"{value}\""
    return f"\"{value}\""

def _num_pydigits(value):
    count = 0
    while value != 0:
        value >>= 15
        count += 1
    return count

def _format_number_token(raw):
    value = ast.literal_eval(raw)
    if isinstance(value, int):
        if -INT64_MAX - 1 <= value <= INT64_MAX:
            return f"{value:d}"
        return f"<bigint:{_num_pydigits(abs(value))}pydigits>"
    if isinstance(value, float):
        return f"{value:f}"
    if isinstance(value, complex):
        # scanner_test currently prints T_FLOAT by reading the float field from
        # a complex object for imaginary literals, which yields the real part.
        return f"{value.real:f}"
    return str(value)

def _is_combining_errortoken(token):
    if token.type != ERRORTOKEN:
        return False
    if len(token.string) != 1:
        return False
    return unicodedata.combining(token.string) != 0

def _emit_name(pending_name):
    linenum = pending_name[0]
    text = pending_name[1]
    normalized = unicodedata.normalize("NFKC", text)
    print(f"{linenum}: `{normalized}`")


def main():
    # Tokenize using python tokenize module; report in the same format as
    # scanner_test.
    filename = sys.argv[1]
    adjust_eof_line = False
    pending_name = None
    with open(filename, "rb") as fp:
        for token in tokenize(fp.readline):
            type, string, start, end, line = token
            if type in (COMMENT, NL, ENCODING):
                continue
            if type == NAME:
                if pending_name is not None:
                    _emit_name(pending_name)
                pending_name = [end[0], string, end, line]
                continue
            if pending_name is not None:
                if (
                    _is_combining_errortoken(token)
                    and start == pending_name[2]
                    and line == pending_name[3]
                ):
                    pending_name[1] += string
                    pending_name[2] = end
                    continue
                _emit_name(pending_name)
                pending_name = None

            linenum = end[0]
            if type == OP:
                t = f"`{string}`"
            elif type == NUMBER:
                t = _format_number_token(string)
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
                t = _format_string_token(string)
            else:
                t = f"<unknown>: {token}"
            print(f"{linenum}: {t}")
        if pending_name is not None:
            _emit_name(pending_name)

if __name__ == "__main__":
    main()
