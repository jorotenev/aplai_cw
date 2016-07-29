problems=("verydifficult" "expert" "lambda" "hard17" "symme" "eastermonster" "tarek_052" "goldennugget" "coloin" "extra1" "extra2" "extra3" "extra4" "inkara2012" "clue18" "clue17" "sudowiki_nb28" "sudowiki_nb49" "veryeasy")
testt=("verydifficult" "expert")
iteration=$1

pwd
rm -rf log$iteration 
mkdir log$iteration
limit=$2
for i in ${problems[@]}; do
	output_file=log$iteration/output_$i.log
	echo $i
	echo "begin-- $i" >> $output_file
	echo "Timeout is $limit" >> $output_file
	timeout --foreground -k 3 $limit swipl -q -s chr_sudoku.pl -t "solve($i)." &>> $output_file
	echo "end-- $i" >> $output_file
done