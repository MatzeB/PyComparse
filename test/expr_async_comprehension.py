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
        value = self.i
        self.i += 1
        return value


async def main():
    list_comp = [x async for x in ARange(6) if x % 2 == 0]
    set_comp = {x async for x in ARange(6) if x % 2 == 1}
    dict_comp = {x: x + 1 async for x in ARange(3)}
    gen_expr = (x async for x in ARange(5) if x > 2)

    collected = []
    async for x in gen_expr:
        collected.append(x)

    print(list_comp)
    print(sorted(set_comp))
    print(dict_comp)
    print(collected)


loop = asyncio.new_event_loop()
loop.run_until_complete(main())
loop.close()
