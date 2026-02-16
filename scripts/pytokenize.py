#!/usr/bin/env python3
import ast
import io
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


def _emit(line, token_text):
    print(f"{line}: {token_text}")


def _split_string_literal(raw):
    quote_pos = -1
    for i, ch in enumerate(raw):
        if ch in ("'", '"'):
            quote_pos = i
            break
    if quote_pos < 0:
        return None

    prefix = raw[:quote_pos]
    quote = raw[quote_pos]
    if raw.startswith(quote * 3, quote_pos):
        return prefix, quote, raw[quote_pos + 3 : -3]
    return prefix, quote, raw[quote_pos + 1 : -1]


def _is_fstring_literal(raw):
    parts = _split_string_literal(raw)
    if parts is None:
        return False
    prefix, _, _ = parts
    return "f" in prefix.lower()


def _decode_hex_escape(content, i, digits):
    j = i + 2
    value = 0
    for _ in range(digits):
        if j >= len(content):
            return "\ufffc", j
        c = content[j]
        if "0" <= c <= "9":
            d = ord(c) - ord("0")
        elif "a" <= c <= "f":
            d = 10 + ord(c) - ord("a")
        elif "A" <= c <= "F":
            d = 10 + ord(c) - ord("A")
        else:
            return "\ufffc", j
        value = (value << 4) | d
        j += 1
    if value > 0x10FFFF:
        value = 0xFFFC
    return chr(value), j


def _decode_named_escape(content, i):
    # content[i:i+2] == "\\N"
    j = i + 2
    if j >= len(content) or content[j] != "{":
        return "\\N", j
    j += 1
    name_start = j
    while j < len(content) and content[j] != "}":
        if content[j] in ("\n", "\r", "'", '"'):
            break
        j += 1
    if j >= len(content) or content[j] != "}" or j == name_start:
        return "\ufffc", j
    name = content[name_start:j]
    j += 1
    try:
        return unicodedata.lookup(name), j
    except KeyError:
        return "\ufffc", j


def _consume_nonraw_literal_char(content, i, line):
    c = content[i]
    if c == "\r":
        i += 1
        if i < len(content) and content[i] == "\n":
            i += 1
        return "\n", i, line + 1
    if c == "\n":
        return "\n", i + 1, line + 1
    if c != "\\":
        return c, i + 1, line
    if i + 1 >= len(content):
        return "\\", i + 1, line

    esc = content[i + 1]
    if esc in ("\\", "'", '"'):
        return esc, i + 2, line
    if esc == "a":
        return "\a", i + 2, line
    if esc == "b":
        return "\b", i + 2, line
    if esc == "f":
        return "\f", i + 2, line
    if esc == "n":
        return "\n", i + 2, line
    if esc == "r":
        return "\r", i + 2, line
    if esc == "t":
        return "\t", i + 2, line
    if esc == "v":
        return "\v", i + 2, line
    if esc in "01234567":
        j = i + 1
        value = 0
        count = 0
        while j < len(content) and count < 3 and content[j] in "01234567":
            value = (value << 3) | (ord(content[j]) - ord("0"))
            j += 1
            count += 1
        if value > 0x10FFFF:
            value = 0xFFFC
        return chr(value), j, line
    if esc == "x":
        value, j = _decode_hex_escape(content, i, 2)
        return value, j, line
    if esc == "u":
        value, j = _decode_hex_escape(content, i, 4)
        return value, j, line
    if esc == "U":
        value, j = _decode_hex_escape(content, i, 8)
        return value, j, line
    if esc == "N":
        value, j = _decode_named_escape(content, i)
        return value, j, line
    if esc == "\n":
        return "", i + 2, line + 1
    if esc == "\r":
        j = i + 2
        if j < len(content) and content[j] == "\n":
            j += 1
        return "", j, line + 1
    return "\\" + esc, i + 2, line


def _consume_raw_literal_char(content, i, line):
    c = content[i]
    if c == "\r":
        i += 1
        if i < len(content) and content[i] == "\n":
            i += 1
        return "\n", i, line + 1
    if c == "\n":
        return "\n", i + 1, line + 1
    if c != "\\":
        return c, i + 1, line
    if i + 1 >= len(content):
        return "\\", i + 1, line
    next_c = content[i + 1]
    if next_c in ("{", "}"):
        # Raw f-strings do not escape braces with backslash.
        return "\\", i + 1, line
    if next_c == "\n":
        return "\\\n", i + 2, line + 1
    if next_c == "\r":
        j = i + 2
        if j < len(content) and content[j] == "\n":
            j += 1
        return "\\\n", j, line + 1
    return "\\" + next_c, i + 2, line


def _detect_string_start(text, i):
    if i >= len(text):
        return None

    if text[i] in ("'", '"'):
        quote = text[i]
        return (i, "", quote, text.startswith(quote * 3, i), False)

    if not text[i].isalpha():
        return None

    j = i
    while j < len(text) and text[j].isalpha() and (j - i) < 3:
        j += 1
    if j >= len(text) or text[j] not in ("'", '"'):
        return None
    prefix = text[i:j]
    if not prefix:
        return None
    if any(c not in "rRbBuUfF" for c in prefix):
        return None
    quote = text[j]
    return (j, prefix, quote, text.startswith(quote * 3, j), "r" in prefix.lower())


def _skip_python_string(text, i):
    det = _detect_string_start(text, i)
    if det is None:
        return i + 1, 0

    quote_pos, prefix, quote, triple, raw = det
    j = quote_pos + (3 if triple else 1)
    newlines = 0

    while j < len(text):
        if triple and text.startswith(quote * 3, j):
            return j + 3, newlines

        c = text[j]
        if not triple and c == quote:
            return j + 1, newlines

        if c == "\n":
            newlines += 1

        if c == "\\":
            if raw:
                if j + 1 < len(text) and text[j + 1] == quote:
                    j += 2
                    continue
            else:
                if j + 1 < len(text):
                    if text[j + 1] == "\n":
                        newlines += 1
                        j += 2
                        continue
                    if text[j + 1] == "\r":
                        if j + 2 < len(text) and text[j + 2] == "\n":
                            newlines += 1
                            j += 3
                            continue
                        newlines += 1
                        j += 2
                        continue
                    j += 2
                    continue
        j += 1

    return len(text), newlines


def _extract_replacement_field(content, i, line):
    start = i
    brace_depth = 0
    while i < len(content):
        det = _detect_string_start(content, i)
        if det is not None:
            i, consumed_newlines = _skip_python_string(content, i)
            line += consumed_newlines
            continue

        c = content[i]
        if c == "\n":
            line += 1
            i += 1
            continue
        if c == "{":
            brace_depth += 1
            i += 1
            continue
        if c == "}":
            if brace_depth == 0:
                return content[start:i], i + 1, line
            brace_depth -= 1
            i += 1
            continue
        i += 1

    return content[start:], i, line


def _is_debug_equal(field, i):
    if i + 1 < len(field) and field[i + 1] == "=":
        return False
    j = i - 1
    while j >= 0 and field[j].isspace():
        j -= 1
    if j >= 0 and field[j] in ("<", ">", "!", "=", ":"):
        return False
    return True


def _split_replacement_field(field):
    n = len(field)
    paren = 0
    bracket = 0
    brace = 0
    i = 0

    expr_end = n
    debug_idx = None
    conv_idx = None
    conv_char = None
    colon_idx = None

    while i < n:
        det = _detect_string_start(field, i)
        if det is not None:
            i, _ = _skip_python_string(field, i)
            continue

        c = field[i]
        if c == "(":
            paren += 1
            i += 1
            continue
        if c == "[":
            bracket += 1
            i += 1
            continue
        if c == "{":
            brace += 1
            i += 1
            continue
        if c == ")" and paren > 0:
            paren -= 1
            i += 1
            continue
        if c == "]" and bracket > 0:
            bracket -= 1
            i += 1
            continue
        if c == "}" and brace > 0:
            brace -= 1
            i += 1
            continue

        if paren == 0 and bracket == 0 and brace == 0:
            if c == "=" and _is_debug_equal(field, i):
                debug_idx = i
                expr_end = i
                i += 1
                while i < n and field[i].isspace():
                    i += 1
                if i < n and field[i] == "!":
                    conv_idx = i
                    conv_char = field[i + 1] if i + 1 < n else ""
                    i += 2
                if i < n and field[i] == ":":
                    colon_idx = i
                break
            if c == "!":
                conv_idx = i
                expr_end = i
                conv_char = field[i + 1] if i + 1 < n else ""
                i += 2
                if i < n and field[i] == ":":
                    colon_idx = i
                break
            if c == ":":
                colon_idx = i
                expr_end = i
                break

        i += 1

    format_spec = None
    if colon_idx is not None:
        format_spec = field[colon_idx + 1 :]

    return {
        "expr": field[:expr_end],
        "debug_idx": debug_idx,
        "conv_idx": conv_idx,
        "conv_char": conv_char,
        "colon_idx": colon_idx,
        "format_spec": format_spec,
    }


def _line_at_index(base_line, text, idx):
    if idx is None:
        return base_line
    return base_line + text.count("\n", 0, idx + 1)


def _emit_expr_tokens(expr_text, start_line):
    if expr_text.strip() == "":
        return

    source = ("\n" * (start_line - 1)) + expr_text
    reader = io.BytesIO(source.encode("utf-8")).readline
    for token in tokenize(reader):
        token_type = token.type
        if token_type in (ENCODING, COMMENT, NL, NEWLINE, INDENT, DEDENT, ENDMARKER):
            continue
        linenum = token.end[0]
        if token_type == OP:
            _emit(linenum, f"`{token.string}`")
        elif token_type == NAME:
            _emit(linenum, f"`{unicodedata.normalize('NFKC', token.string)}`")
        elif token_type == NUMBER:
            _emit(linenum, _format_number_token(token.string))
        elif token_type == STRING:
            _emit_string_token(token.string, token.start[0], linenum)
        else:
            _emit(linenum, f"<unknown>: {token}")


def _emit_replacement_field(field_text, field_start_line, raw):
    parts = _split_replacement_field(field_text)

    _emit_expr_tokens(parts["expr"], field_start_line)

    debug_idx = parts["debug_idx"]
    if debug_idx is not None:
        _emit(_line_at_index(field_start_line, field_text, debug_idx), "`=`")

    conv_idx = parts["conv_idx"]
    conv_char = parts["conv_char"]
    if conv_idx is not None:
        line = _line_at_index(field_start_line, field_text, conv_idx)
        _emit(line, "`!`")
        if conv_char:
            _emit(line, f"`{conv_char}`")

    colon_idx = parts["colon_idx"]
    if colon_idx is not None:
        colon_line = _line_at_index(field_start_line, field_text, colon_idx)
        _emit(colon_line, "`:`")
        format_spec = parts["format_spec"]
        if format_spec is None:
            format_spec = ""
        _emit_fstring_content(format_spec, colon_line, raw)


def _emit_fstring_content(content, start_line, raw):
    i = 0
    line = start_line
    literal_chars = []
    saw_field = False

    while i < len(content):
        c = content[i]
        if c == "{":
            if i + 1 < len(content) and content[i + 1] == "{":
                literal_chars.append("{")
                i += 2
                continue

            text = "".join(literal_chars)
            rendered = _render_bytes_for_scanner(text.encode("utf-8"))
            kind = "f-string start" if not saw_field else "f-string fragment"
            _emit(line, f'{kind} "{rendered}"')
            literal_chars = []
            i += 1

            field_start_line = line
            field_text, i, line = _extract_replacement_field(content, i, line)
            _emit_replacement_field(field_text, field_start_line, raw)
            saw_field = True
            continue

        if c == "}":
            if i + 1 < len(content) and content[i + 1] == "}":
                literal_chars.append("}")
                i += 2
                continue
            literal_chars.append("}")
            i += 1
            continue

        if raw:
            decoded, i, line = _consume_raw_literal_char(content, i, line)
        else:
            decoded, i, line = _consume_nonraw_literal_char(content, i, line)
        literal_chars.append(decoded)

    text = "".join(literal_chars)
    rendered = _render_bytes_for_scanner(text.encode("utf-8"))
    if saw_field:
        _emit(line, f'f-string end "{rendered}"')
    else:
        _emit(line, f'"{rendered}"')


def _emit_string_token(raw, start_line, end_line):
    if _is_fstring_literal(raw):
        parts = _split_string_literal(raw)
        assert parts is not None
        prefix, _, content = parts
        raw_flag = "r" in prefix.lower()
        _emit_fstring_content(content, start_line, raw_flag)
        return
    _emit(end_line, _format_string_token(raw))


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
    _emit(linenum, f"`{normalized}`")


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
                _emit_string_token(string, start[0], linenum)
                continue
            else:
                t = f"<unknown>: {token}"
            _emit(linenum, t)
        if pending_name is not None:
            _emit_name(pending_name)

if __name__ == "__main__":
    main()
