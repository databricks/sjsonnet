def compute():
    res = 0
    # A loop large enough to be measured, but small enough for a benchmark
    for i in range(1000000):
        res = (res + i) % 1000000
    return res

# Top-level execution
X = compute()
