import asyncio


async def arange(n):
    for i in range(n):
        yield i


def make_async_for(n):
    return (i * 2 async for i in arange(n))


async def wrap(n):
    return n


def make_await_filter(n):
    return (i * 2 for i in range(n) if await wrap(i))


async def run():
    print([i async for i in make_async_for(6)])
    print([i async for i in make_await_filter(6)])


loop = asyncio.new_event_loop()
loop.run_until_complete(run())
loop.close()
