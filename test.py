
import dis

def new_int(i, j):
    a = 10
    return int(i + j + a)

dis.dis(new_int)
dis.disassemble(new_int)

print(dis.code_info(new_int))
