import sys

def convert(s):
    s = s.replace("halt","0")
    s = s.replace("input","1")
    s = s.replace("move","2")
    s = s.replace("not","3")
    s = s.replace("or","4")
    s = s.replace("and","5")
    s = s.replace("add","6")
    s = s.replace("sub","7")
    s = s.replace("mul","8")
    s = s.replace("div","9")
    s = s.replace("mod","10")
    s = s.replace("eq","11")
    s = s.replace("gt","12")
    s = s.replace("if","13")
    s = s.replace("goto","14")
    s = s.replace("output","15")
    s = s.replace("read","16")
    s = s.replace("_", "37")
    return s

def main():
    print(convert(sys.stdin.read()), end="")

    
if __name__ == '__main__':
    main()
