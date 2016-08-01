# Solver for the Shikaku puzzles.  
Example puzzles are in the shikakuproblems.pl file.  
To run a single puzzle (assuming Swi Prolog is used):  
`$ swipl -s chr_shikaku.pl -t 'solve(helmut)'`  
helmut can be replace with any puzzle from the shikakuproblems.pl file.  

Use the `run_script.sh` to run all puzzles  
`$ ./run_script.sh <iteration:any> <timeout:int>`  
iteration - a symbol(s) to mark the version of the solver. handy to compare the performance at different stages of implementation  
timeout - in seconds, the allowed period of time to solve a puzzle, before moving to the next one.  
Example usage:  
`$ ./run_script.py 1 60` - the output will be in log/log1, and the timeout limit is 60 seconds.

After running the above script, one can use `$ python parse_log.py log/log1` to generate a latex file with a table with the results of all puzzles. `log1` to stick with the example usage of ` run_chr.sh` where `1` was passed as iteration.  After running the `parse_log` file, a latex file is places in the argument folder, with a table with the results of the run combined.
