problems=("verydifficult" "expert" "lambda" "hard17" "symme" "eastermonster" "tarek_052" "goldennugget" "coloin" "extra1" "extra2" "extra3" "extra4" "inkara2012" "clue18" "clue17" "sudowiki_nb28" "sudowiki_nb49" "veryeasy")
testt=("expert")
iteration=$1


limit=$2
viewpoint=$3
rm -rf $viewpoint/log$iteration 
mkdir $viewpoint/log$iteration
for i in ${problems[@]}; do
	output_file=$viewpoint/log$iteration/output_$i.log
	echo $i
	echo "begin-- $i" >> $output_file
	echo "Timeout is $limit" >> $output_file
	file="chr"$viewpoint"_sudoku.pl"
	timeout --foreground -k 3 $limit swipl -q -s $file -t "solve($i)." &>> $output_file
	echo "end-- $i" >> $output_file
done