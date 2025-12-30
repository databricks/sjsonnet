import math

def makeArray(n, func):
    return [func(i) for i in range(int(n))]

def pow_bench(n):
    n_int = int(n)
    res = 0
    for i in range(n_int):
        res = 3 ** 2
    return res

def floor_bench(n):
    n_int = int(n)
    res = 0
    for i in range(n_int):
        res = math.floor(10.99999)
    return res

def ceil_bench(n):
    n_int = int(n)
    res = 0
    for i in range(n_int):
        res = math.ceil(10.99999)
    return res

def sqrt_bench(n):

    n_int = int(n)

    res = 0

    for i in range(n_int):

        res = math.sqrt(16)

    return res



def filter_bench(n):

    n_int = int(n)

    return [x for x in range(1, n_int + 1) if x % 2 == 0]



def map_bench(n):

    n_int = int(n)

    return [x * x for x in range(1, n_int + 1)]





def filter_bench(n):

    n_int = int(n)

    return [x for x in range(1, n_int + 1) if x % 2 == 0]



def map_bench(n):

    n_int = int(n)

    return [x * x for x in range(1, n_int + 1)]
