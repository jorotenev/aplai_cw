import sys,os,re
from tabulate import tabulate
from decimal import Decimal


file_prefix = 'output_'
logDir = sys.argv[1] + '/'

timeoutPattern = re.compile("^Timeout is (\d+)$",re.MULTILINE)
generationPattern = re.compile('^% (\S+) inferences, .+ in (\S+) seconds')
searchPattern = re.compile("^% (\d+\.\d+|\d+) seconds cpu time for (\S+) inferences")

def getLineStartingWith(contentList, lineprefix):
	return [c for c in contentList if c.startswith(lineprefix)][0]

def getTimeout(content):
	try:
		line = getLineStartingWith(content,'Timeout is ')
		p = timeoutPattern
		a = re.match(p, line).groups()
		return a[0]
	except Exception as ex:
		print(ex)

def readFile(file):
	with open(logDir+file, 'r') as logFile:
	    data=logFile.readlines()
	    return [s.strip() for s in data]

def problemGenerationInferencesAndTime(content):
	line = content[3]
	assert 'inferences' in line and 'CPU' in line

	p = generationPattern
	a = re.match(p,line).groups()
	return a[1],a[0]

def getProblemName(content):
	return content[0].replace('begin-- ', '')

def getSearchStatistics(content, genSecs):
	try:
		line = content[6]
		p = searchPattern
		seconds,inferences = re.match(p, line).groups()
		assert seconds and inferences
		seconds = Decimal(seconds)
		genSecs =Decimal(genSecs)
		seconds = seconds - genSecs

		return seconds, inferences

	except Exception as ex:
		# print(ex)
		return "FAIL", "FAIL"

def runBaby(files, logDir):
	files.sort()
	lines = []
	timeout = -1
	for file in files:
		logContent = readFile(file)
		problemName = getProblemName(logContent)
		# print(problemName)

		timeout = getTimeout(logContent) 
		timeout = timeout if timeout else -1
		# print(timeout)
		genSecs, genInf = problemGenerationInferencesAndTime(logContent)
		# print(genSecs, genInf)

		searchSecs,searchInf = getSearchStatistics(logContent,genSecs)
		# print(searchSecs,searchInf)
		lines.append([problemName, genSecs, searchSecs])


	headers = ['Name', 'Problem generation time', 'Search Time']
	table =  tabulate(lines, headers=headers, tablefmt='latex_booktabs')
	iteration = logDir.replace('log', "").replace('/','')
	caption ='\caption{%s}' % 'Iteration %s. Time is in seconds. Maximum run-time limit is %s '%(iteration, timeout)+'\n'
	label = '\label{tab:%s}'%iteration+'\n'
	table = '\\begin{table}\n' +caption+ label+table + '\end{table}'
	with open( logDir+'RESULT.latex','w') as save_file:
		print(table, file=save_file) 	
		# print(table) 	



if __name__ == '__main__':
	files = [f for f in os.listdir(logDir) if f.startswith(file_prefix)]
	runBaby(files, logDir)