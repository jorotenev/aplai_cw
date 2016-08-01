The sudoku puzzles are in the `sudoku_problems.pl` file.  
Two different viewpoints of solving the puzzles are presented - `chrVP1_sudoku.pl` and `chrVP2_sudoku.pl`. 
VP1(viewpoint #1) is the classical viewpoint and VP2 uses sets (reference of the approach is on the top of the chrVP2_sudoku.pl file).

Use the ./run_chr.sh script to solve all puzzles.  
`./run_chr.sh <iteration:any> <timeout:int> <viewpoint:[VP1|VP2]>`  
<iteration> is a alphanumerical string used to mark different versions of the program being ran (handy to compare the efficiency at different stages of the implementation). It will be used to name the output directory for the log.
<timeout> is the runtime limit, in seconds, after which the process for a given puzzle will be killed
<viewpoint> is the viewpoint to use (VP1 and VP2 are the current valid options).  
Example usage  
`./run_chr.sh boo 60 VP2` which will run the `chrVP1_sudoku.pl` file with a 60 second timeout limit for each puzzle, and the resutls will be stored under VP2/log_boo

To check the results, after running the above script, go to the VP2/log_boo folder
