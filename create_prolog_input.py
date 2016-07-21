posibles = list(range(1,10))
buckets = {}
maybes = []
for i in range(1,10):
	buckets[i] = []

line = "..7..9....6.8...1..9.23...8...7.46..7.6...9.3..56.3...6...81.9..1...2.4....3..56."	
def pos(i):
	row, col = divmod(i,9)
	col = col if col > 0 else 1
	return row + 1 , col
arr1 = []
row, col = 1,1
for i, ch in enumerate(line):
	if col == 10:
		col = 1
		row = row + 1
	if  ch != '.':
		buckets[int(ch)].append("%i-%i" % (row, col))
	else:
		maybes.append("maybe(%i-%i, %s)" % (row,col,str(posibles)))
	col = col + 1
# combine
[arr1.append("bucket(%i, %s)" % (i, buckets[i])) for i in buckets.keys() ]
arr1.sort()
maybes.sort()
arr1 = arr1 + maybes

s = ''
for i, el1 in enumerate(arr1):
	s = s + el1 
	s = s + ', ' if i != len(arr1) - 1 else s + '.'
s = s.replace('\'',"")
print(s)

a = ["given(9-9,1)",
"given(5-5,5)",
"given(4-5,2)",
"given(6-5,9)",
"given(4-1,1)",
"given(4-2,3)",
"given(5-2,4)",
"given(6-8,7)",
"given(6-2,8)",
"given(6-1,2)",
"given(9-1,9)",
"given(9-2,2)",
"given(9-3,8)",
"given(4-3,9)",
"given(4-8,8)",
"given(5-8,2)",
"given(5-6,8)",
"given(2-5,7)",
"given(2-6,5)",
"given(9-5,4)",
"given(9-6,7)",
"given(3-6,6)",
"given(3-8,5)",
"given(2-9,9)",
"given(4-9,5)",
"given(6-9,4)",
"given(6-7,1)",
"given(2-7,4)",
"given(3-7,7)",
"given(1-5,1)",
"given(1-9,6)",
"given(1-8,3)",
"given(1-2,5)",
"given(1-1,8)",
"given(2-1,3)",
"given(3-1,4)",
"given(3-3,1)",
"given(1-7,2)",
"given(1-4,4)",
"given(5-4,1)",
"given(2-3,2)",
"given(7-2,7)",
"given(8-1,5)",
"given(8-3,3)",
"given(8-5,6)",
"given(8-4,9)",
"given(7-4,5)",
"given(7-3,4)",
"given(8-7,8)",
"given(7-7,3)",
"given(7-9,2)",
"given(8-9,7)",
"given(9-8,6)",
"given(9-7,5)",
"given(9-4,3)",
"given(8-8,4)",
"given(8-6,2)",
"given(8-2,1)",
"given(7-8,9)",
"given(7-6,1)",
"given(7-5,8)",
"given(7-1,6)",
"given(6-6,3)",
"given(6-4,6)",
"given(6-3,5)",
"given(5-9,3)",
"given(5-7,9)",
"given(5-3,6)",
"given(5-1,7)",
"given(4-7,6)",
"given(4-6,4)",
"given(4-4,7)",
"given(3-9,8)",
"given(3-5,3)",
"given(3-4,2)",
"given(3-2,9)",
"given(2-8,1)",
"given(2-4,8)",
"given(2-2,6)",
"given(1-6,9)",
"given(1-3,7)"]



a.sort()
# for b in a:
	# print(b)