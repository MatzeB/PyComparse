for i in range(30):
    if i % 15 == 0:
        print("fizzbuzz", "", "")
        for x in range(i):
            if x % 4:
                continue
            if x > 9:
                break
            print(".", "", "")
        print()
        continue
    if i % 3 == 0:
        print("fizz")
        continue
    if i % 5 == 0:
        print("buzz")
        continue
    print(i)
    if i > 21:
        break
