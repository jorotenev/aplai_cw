import re,sys,os
logDir = sys.argv[1] + '/'
file_prefix='output_'
def readFile(file):
	with open(logDir+file, 'r') as logFile:
	    data=logFile.readlines()
	    return [s.strip() for s in data if s.startswith("given(")]

p = re.compile('.+(\d)\)')
def parse(line):
	a = re.match(p, line).groups()[0]
	return a

 

files = [f for f in os.listdir(logDir) if f.startswith(file_prefix)]

for file in files:
	content = readFile(file)
	content.sort()
	res = [parse(ch) for ch in content if ch]
	res = ''.join(res)
	saveName = file.replace('output','answer')
	fileName = logDir+'answers/'+saveName
	os.makedirs(os.path.dirname(fileName), exist_ok=True)
	with open(fileName, 'w') as saveFile:
		print(res,file=saveFile)
