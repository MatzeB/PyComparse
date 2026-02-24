class Proc:
    def __init__(self, lines):
        self.stdout = lines

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):
        return False


def find_value(get_index):
    first = None
    try:
        proc = Proc(
            [
                b"cscotun0  Link encap:UNSPEC  HWaddr 00-00-00-00-00-00\n",
                b"eth0      Link encap:Ethernet  HWaddr 12:34:56:78:90:ab\n",
            ]
        )
        if not proc:
            return None
        with proc:
            for line in proc.stdout:
                words = line.lower().rstrip().split()
                for i in range(len(words)):
                    if words[i] in (b"hwaddr",):
                        try:
                            word = words[get_index(i)]
                            value = int(word.replace(b":", b""), 16)
                            if value:
                                return value
                            first = first or value
                        except (ValueError, IndexError):
                            pass
    except OSError:
        pass
    return first or None


for _ in range(5):
    print(find_value(lambda i: i + 1))
