
import re
# buckets = [''] * 82
buckets = []

inputStr = """
bucket(2,[9-9,8-3,7-6,6-4,5-7,4-1,3-5,2-8,1-2])
bucket(4,[9-8,8-2,7-5,6-6,5-9,4-3,3-7,2-1,1-4])
bucket(6,[9-7,8-1,7-4,6-8,5-2,4-5,3-9,2-3,1-6])
bucket(1,[9-6,8-9,7-3,6-5,5-8,4-2,3-4,2-7,1-1])
bucket(3,[9-5,8-8,7-2,6-7,5-1,4-4,3-6,2-9,1-3])
bucket(5,[9-4,8-7,7-1,6-9,5-3,4-6,3-8,2-2,1-5])
bucket(8,[9-3,8-6,7-9,6-1,5-4,4-7,3-2,2-5,1-8])
bucket(7,[9-2,8-5,7-8,6-3,5-6,4-9,3-1,2-4,1-7])
bucket(9,[9-1,8-4,7-7,6-2,5-5,4-8,3-3,2-6,1-9])
"""


inputStr = inputStr.replace('bucket(','')
inputStr = inputStr.replace(')','')
inputStr = re.sub('\[|\]','', inputStr)


inputStr = inputStr.splitlines()

temp = []
for x in inputStr:
	if not x:
		continue
	print(x)
	bucket = x[0]
	coords = x[2:]
	for c in coords.split(','):
		row = int(c[0])
		col = int(c[2])
		pos = row * col
		# print('%i - %i = %i') % (row, col, pos)
		buckets.append((c, bucket))

buckets.sort()
s = ''
for pos,n in buckets:
	s =s+n
print(s)

# print(''.join(buckets))

with open("Output.txt", "w") as text_file:
    text_file.write(s)
