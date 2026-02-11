import asyncio


events = []


class CM:
    async def __aenter__(self):
        events.append("enter")
        return 41

    async def __aexit__(self, exc_type, exc, tb):
        events.append("exit")
        return False


async def main():
    async with CM() as value:
        print(value + 1)
    print(",".join(events))


loop = asyncio.new_event_loop()
loop.run_until_complete(main())
loop.close()
