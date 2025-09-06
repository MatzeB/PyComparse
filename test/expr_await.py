import asyncio
import time

async def jo(name, inp=44):
    print("jo ", name, "before sleep")
    await asyncio.sleep(0)
    print("jo ", name, "after sleep")
    return inp+3


async def sum_(begin, end):
    for i in range(begin, end):
        print("I: ", i)
        r = await jo("joe", i)
        print("R: ", r)

loop = asyncio.new_event_loop()
tasks = [
    loop.create_task(sum_(0, 10)),
    loop.create_task(sum_(33, 37)),
    loop.create_task(jo("dawg"))
]
loop.run_until_complete(asyncio.wait(tasks))
loop.close()
