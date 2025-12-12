def exec(a):
    b = 0
    d = 7 * 365 + a
    a = d

    num_match = 0
    expect = 0
    out = []
    while True:
        t = a
        a = t // 2
        b = t % 2
        out = b
        if out == expect:
            num_match += 1
            if num_match == 100:
                return True
            expect = 1 - expect
        else:
            return False
        if a == 0:
            a = d


for a in range(0, 1_000_000):
    if exec(a):
        print(a)
        break
