class ImmutableObj:
    def __init__(self, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10):
        self.f1 = f1; self.f2 = f2; self.f3 = f3; self.f4 = f4; self.f5 = f5
        self.f6 = f6; self.f7 = f7; self.f8 = f8; self.f9 = f9; self.f10 = f10
    
    def withF1(self, v): return ImmutableObj(v, self.f2, self.f3, self.f4, self.f5, self.f6, self.f7, self.f8, self.f9, self.f10)
    def withF2(self, v): return ImmutableObj(self.f1, v, self.f3, self.f4, self.f5, self.f6, self.f7, self.f8, self.f9, self.f10)
    def withF3(self, v): return ImmutableObj(self.f1, self.f2, v, self.f4, self.f5, self.f6, self.f7, self.f8, self.f9, self.f10)
    def withF4(self, v): return ImmutableObj(self.f1, self.f2, self.f3, v, self.f5, self.f6, self.f7, self.f8, self.f9, self.f10)
    def withF5(self, v): return ImmutableObj(self.f1, self.f2, self.f3, self.f4, v, self.f6, self.f7, self.f8, self.f9, self.f10)
    def withF6(self, v): return ImmutableObj(self.f1, self.f2, self.f3, self.f4, self.f5, v, self.f7, self.f8, self.f9, self.f10)
    def withF7(self, v): return ImmutableObj(self.f1, self.f2, self.f3, self.f4, self.f5, self.f6, v, self.f8, self.f9, self.f10)
    def withF8(self, v): return ImmutableObj(self.f1, self.f2, self.f3, self.f4, self.f5, self.f6, self.f7, v, self.f9, self.f10)
    def withF9(self, v): return ImmutableObj(self.f1, self.f2, self.f3, self.f4, self.f5, self.f6, self.f7, self.f8, v, self.f10)
    def withF10(self, v): return ImmutableObj(self.f1, self.f2, self.f3, self.f4, self.f5, self.f6, self.f7, self.f8, self.f9, v)

class MutableObj:
    def __init__(self, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10):
        self.f1 = f1; self.f2 = f2; self.f3 = f3; self.f4 = f4; self.f5 = f5
        self.f6 = f6; self.f7 = f7; self.f8 = f8; self.f9 = f9; self.f10 = f10

def benchmark_immutable(iterations):
    obj = ImmutableObj(0,0,0,0,0,0,0,0,0,0)
    for i in range(int(iterations)):
        obj = obj.withF1(obj.f1 + i)
        obj = obj.withF2(obj.f2 + i)
        obj = obj.withF3(obj.f3 + i)
        obj = obj.withF4(obj.f4 + i)
        obj = obj.withF5(obj.f5 + i)
        obj = obj.withF6(obj.f6 + i)
        obj = obj.withF7(obj.f7 + i)
        obj = obj.withF8(obj.f8 + i)
        obj = obj.withF9(obj.f9 + i)
        obj = obj.withF10(obj.f10 + i)
    return obj.f1 + obj.f2 + obj.f3 + obj.f4 + obj.f5 + obj.f6 + obj.f7 + obj.f8 + obj.f9 + obj.f10

def benchmark_mutable(iterations):
    obj = MutableObj(0,0,0,0,0,0,0,0,0,0)
    for i in range(int(iterations)):
        obj.f1 += i
        obj.f2 += i
        obj.f3 += i
        obj.f4 += i
        obj.f5 += i
        obj.f6 += i
        obj.f7 += i
        obj.f8 += i
        obj.f9 += i
        obj.f10 += i
    return obj.f1 + obj.f2 + obj.f3 + obj.f4 + obj.f5 + obj.f6 + obj.f7 + obj.f8 + obj.f9 + obj.f10
