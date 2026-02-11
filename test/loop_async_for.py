import asyncio


class ARange:
    def __init__(self, n):
        self.n = n
        self.i = 0

    def __aiter__(self):
        self.i = 0
        return self

    async def __anext__(self):
        if self.i >= self.n:
            raise StopAsyncIteration
        v = self.i
        self.i += 1
        return v


async def main():
    total = 0
    async for x in ARange(4):
        total += x
    print(total)

    async for _ in ARange(0):
        print("bad")
    else:
        print("else")


loop = asyncio.new_event_loop()
loop.run_until_complete(main())
loop.close()
