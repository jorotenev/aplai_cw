require 'rubygems'
require 'gecoder'

class SudokuProblem
    include Gecode::Mixin

    def initialize(hints)
        sudoku_is_an int_var_matrix(9, 9, 1..9)
        (0..8).each do |row|
            (0..8).each do |col|
                unless hints[row,col] == 0
                    sudoku[row,col].must == hints[row,col]
                end 
            end
        end
        (0..8).each do |row|
            sudoku.row(row).must_be.distinct
        end
        (0..8).each do |col|
            sudoku.column(col).must_be.distinct
        end 
        ranges = [(0..2),(3..5),(6..8)]
        ranges.each do |x|
            ranges.each do |y|
                block = sudoku.minor(x,y)
                block.must_be.distinct
            end
        end
    branch_on sudoku
    end
    def to_s
        puts "SIze of array is #{sudoku.values.size}"
        row = Array.new
        sudoku.values.each_with_index do |val,i|
            row.push(val)
            if row.size == 9
                puts row.join(", ")
                row = Array.new
            end
            # puts sudoku.values[0..3]
        end
    end
end

h = Matrix[
    [0,0,0,0,5,3,0,6,0],
    [0,0,5,0,9,0,0,0,0],
    [4,0,3,2,0,7,0,9,0],
    [9,0,2,0,3,0,0,8,0],
    [0,0,8,0,0,0,5,0,0],
    [0,7,0,0,4,0,2,0,1],
    [0,3,0,5,0,9,7,0,6],
    [0,0,0,0,1,0,4,0,0],
    [0,6,0,7,8,0,0,0,0]
]
puts SudokuProblem.new(h).solve!