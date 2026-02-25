async def f(it):
    g = lambda: [x async for x in it]
