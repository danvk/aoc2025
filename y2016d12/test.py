def run(b):
    a = 0
    loop = True
    while loop:
        c = 2
        while c != 0:
            if b == 0:
                return (a, c)
            b -= 1
            c -= 1
        a += 1


def run2(b):
    return (b // 2, 2 - b % 2)


for b in range(0, 10):
    print(f"{b=} (a,c)={run(b)}={run2(b)}")
