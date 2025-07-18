import re

a = open("2024/inputs/day3.txt", "r").read()
# a = """xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"""

mults = [*re.finditer(r"mul\((\d{1,3}),(\d{1,3})\)", "do()" + a)]
s = sum([int(m.group(1)) * int(m.group(2)) for m in mults])
print(s)
# 159892596

inst = [*re.finditer(r"(do\(\)|don\'t\(\))", "do()" + a)]

j = s2 = 0
# For every instruction
for i in range(len(inst)):
    # Find the start of the next instruction, or end of string
    n_end = len(a) if i == (len(inst) - 1) else inst[i + 1].start()
    # For every multiplication between instructions
    while (j < len(mults)) and (mults[j].start() < n_end):
        # If the instruction is 'do()' (4 char), add the multiplication to the sum
        sp = inst[i].span()
        if (sp[1] - sp[0]) == 4:
            s2 += int(mults[j].group(1)) * int(mults[j].group(2))
        j += 1
print(s2)
# 92626942


# with open("2024/inputs/day3.txt", "r") as f:
#     a = f.read()
# total1 = total2 = 0
# enabled = True

# for a, b, do, dont in re.findall(r"mul\((\d{1,3}),(\d{1,3})\)|(do\(\))|(don't\(\))", a):
#     if do or dont:
#         enabled = bool(do)
#     else:
#         x = int(a) * int(b)
#         total1 += x
#         total2 += x * enabled

# print(total1, total2)
