require 'rubygems'
require 'gecoder'
 
class ShikakuProblem
    include Gecode::Mixin

    def initialize(grid_size, hints)
        # grid_size - int
        # hints - [ [hint_x,hint_y, hint_value],...  ]
        @hints = hints
        @hint_count = @hints.to_a().size
        @grid_size = grid_size

         # each x,y pair marks the top-left coordiantes of the rects
        @xs = int_var_array(@hint_count, (1..@grid_size))
        @ys = int_var_array(@hint_count, (1..@grid_size))
        # heights and widts
        @ws = int_var_array(@hint_count, (1..@grid_size))
        @hs = int_var_array(@hint_count, (1..@grid_size))

        #hint positions and values
        @hint_xs = @hints.column_vectors[0].to_a()
        @hint_ys = @hints.column_vectors[1].to_a()
        @hint_values = @hints.column_vectors[2].to_a()
 
        # add the constraints for each rect
        @hint_count.times do |i|
             # constraints on where the top-left corner x and y's of the rect can be

            (@xs[i] + @ws[i]).must >= @hint_xs[i] + 1   # eq. to xs[i] >= hint_xs[i] - (ws[i] - 1)

            @xs[i].must <= @hint_xs[i] # max boundary of x
            (@xs[i] + @ws[i]).must <= @grid_size + 1

            (@ys[i] + @hs[i]).must >= @hint_ys[i] + 1   # eq. to ys[i] >= hint_ys[i] - (hs[i] - 1)

            @ys[i].must <= @hint_ys[i]
            (@ys[i] + @hs[i]).must <= @grid_size + 1


            # make sure the rect has the correct size
            (@ws[i] * @hs[i]).must == @hint_values[i]


            # no overlapping
            0.upto(i-1) do |previous_rect|
 
                # That the two squares don't overlap means that i is left of j, 
                # or j is left of i, or i is above j, or j is above i.

                #i is left of j
                ((@xs[previous_rect] - @xs[i]).must >= @ws[i]) | 
                ((@xs[i] - @xs[previous_rect]).must >= @ws[previous_rect]) | 
                ((@ys[previous_rect] - @ys[i]).must >= @hs[i]) | 
                ((@ys[i] - @ys[previous_rect]).must >= @hs[previous_rect])
            end
        end
        # occupied_length_must_equal_total_length(grid_size, grid_size, @xs)
        # # The combined size of all squares occupying a row must be equal to the 
        # # total width.
        # occupied_length_must_equal_total_length(grid_size, grid_size, @ys)


        branch_on @xs 
        branch_on @ys 
        branch_on @ws
        branch_on @hs

    end
  def occupied_length_must_equal_total_length(total_length, axis_length, coords)
    axis_length.times do |i|
      # We create an array of boolean variables and constrain it so that element
      # +j+ is true iff square +j+ occupies +i+ (+i+ being a row or column).
      occupied = bool_var_array(@hint_count)
      occupied.each_with_index do |is_occupying, j|
        coords[j].must_be.in((i - @hint_values[j] + 1)..i, :reify => is_occupying)
      end
      
      # Constrain the combined length of squares that are occupying +i+ to equal
      # the total length. We multiply the sizes with the boolean variables 
      # since a boolean in linear equation is 0 if assigned false and 1 if 
      # assigned true. Hence we will mask out the sizes of any squares not in 
      # +i+.
      occupied_sizes = occupied.zip(@hint_values).map{ |bool, size| bool*size }
      occupied_sizes.inject(0){ |sum, x| x + sum }.must.equal(total_length, 
        :strength => :domain)
    end
  end
    def getHintValuesFromInput(input)
        #returns the last column vector converted to an arr
        input.column_vectors[-1].to_a()
    end
    def to_s
        result = Array.new
        @xs.values.each_with_index do |x,index_x|
            puts "#{x},#{@ys.values[index_x]}:#{@hs.values[index_x]}x#{@ws.values[index_x]}"
            puts "\n"
        end
    end

end #class



# zero-indexed coordinates
# [
#   [x,y,val],
#   ...
# ]
# vals should be sorted

input = Matrix[
    [3,10,3],
    [1,8,4],
    [10,10,4],
    [8,1,5],
    [3,4,6],
    [10,3,6],
    [6,7,6],
    [1,4,8],
    [5,4,8],
    [8,7,8],
    [1,1,9],
    [6,10,9],
    [5,1,12],
    [10,7,12]
]


grid_size=10
ShikakuProblem.new(grid_size, input).solve!.to_s