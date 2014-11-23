# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  # append to super(All_Pieces)
  rotations([[-1,0], [0,0], [1,0], [-1,-1], [0,-1]]),
      [[[-2,0],[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
        [[0,-2],[0, 0], [0, -1], [0, 1], [0, 2]]],
      rotations()


  # your enhancements here

end

class MyBoard < Board
  # your enhancements here

end

class MyTetris < Tetris
  # your enhancements here

end


