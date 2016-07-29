problems=("tiny" "helmut" "p(0,1)" "p(0,2)" "p(0,3)" "p(0,4)" "p(0,5)" "p(1,1)" "p(1,2)" "p(1,3)" "p(1,4)" "p(1,5)" "p(2,1)" "p(2,2)" "p(2,3)" "p(2,4)" "p(2,5)" "p(3,1)" "p(3,2)" "p(3,3)" "p(3,4)" "p(3,5)" "p(4,1)" "p(4,2)" "p(4,3)" "p(4,4)" "p(4,5)" "p(5,1)" "p(5,2)" "p(5,3)" "p(5,4)" "p(5,5)" "p(6,1)")
testt=("tiny" "p(3,1)" )
iteration=$1

mkdir log$iteration
limit=$2
for i in ${problems[@]}; do
	output_file=log$iteration/output_$i.log
	echo $i
	echo "begin-- $i" >> $output_file
	echo "Timeout is $limit" >> $output_file
	timeout --foreground -k 3 $limit swipl -q -s chr_shikaku.pl -t "solve($i)." &>> $output_file
	echo "end-- $i" >> $output_file
done