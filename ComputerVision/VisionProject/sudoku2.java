/* Author
Mohamed Sondo
sudoku, 9×9 int array is used to store all the elements of sudoku.
Instance variable sudoku can be initialized using any of the below two constructors.

All the cells of completely solved sudoku array must have assigned valid values.
sudoku solution having UNASSIGNED i.e. 0 value in any of its cell, is considered to be incomplete or wrong.

*/




/*


Sudoku  is a logic-based combinatorial number-placement puzzle.
Given a partially filled 9×9 2D array grid[9][9],
the goal is to assign digits (from 1 to 9) to the empty cells so that every row, column, and

subgrid of size 3×3 contains exactly one instance of the digits from 1 to 9.


Sudoku can be solved using recursive backtracking algorithm. For other Backtracking algorithms.

What Is Backtracking Algorithm ?

In backtracking algorithms you try to build a solution one step at a time.
If at some step it becomes clear that the current path that you are on cannot lead to a solution you go back to the previous step
(backtrack) and choose a different path. Briefly, once you exhaust all your options at a certain step you go back.

Think of a labyrinth or maze – how do you find a way from an entrance to an exit?
 Once you reach a dead end, you must backtrack. But backtrack to where? to the previous choice point.
  Backtracking is also known as depth-first search.



  Approach For Solving Sudoku Using Recursive Backtracking Algorithm

Like all other Backtracking problems, we can solve Sudoku by one by one assigning numbers to empty cells.
Before assigning a number, we need to confirm that the same number is not present in current row, current column and current 3X3 subgrid.
If number is not present in respective row, column or subgrid, we can assign the number,
and recursively check if this assignment leads to a solution or not. If the assignment doesnâ€™t lead to a solution,
then we try next number for current empty cell. And if none of number (1 to 9) lead to solution, we return false.


*/
class Sudoku
{
    private int[][] sudoku;
    private static final int UNASSIGNED = 0;

    public Sudoku()
    {
        sudoku = new int[9][9];
    }

    public Sudoku(int sudoku[][])
    {
        this.sudoku= sudoku;
    }

    //TO-DO Methods
    //private boolean containsInRow(int row,int number){...}
    //private boolean containsInCol(int col,int number){...}
    //private boolean containsInBox(int row, int col,int number){...}
    //private boolean isAllowed(int row, int col,int number){...}
    //public void displaySudoku(){...}
    //public boolean solveSudoku(){...}
}

//Below three methods are used to check, if number is present in current row, current column and current 3X3 subgrid or not.

private boolean containsInRow(int row,int number)
{
    for(int i=0;i<9;i++)
    {
        if(sudoku[row][i]==number)
        {
            return true;
        }
    }
    return false;
}



private boolean containsInCol(int col,int number)
{
    for(int i=0;i<9;i++)
    {
        if(sudoku[i][col]==number)
        {
            return true;
        }
    }
    return false;
}


private boolean containsInBox(int row, int col,int number)
{
    int r = row - row%3;
    int c = col - col%3;
    for(int i = r ; i< r+3 ; i++)
    {
        for(int j = c; j < c+3 ; j++)
        {
            if(sudoku[i][j]==number)
            {
                return true;
            }
        }

    }
    return false;
}

/*  isAllowed() method uses above three methods
to check if it is safe to assign number in the cell.
 If any of above three method return true, it means particular
 number is not allowed in that cell.*/


 private boolean isAllowed(int row, int col,int number)
{
    return !(containsInRow(row, number) || containsInCol(col, number) || containsInBox(row, col, number));
}





/*

solveSudoku() method starts traversing from top left cell to the right side.
It checks cells in each row one by one and picks up first cell with UNASSIGNED value.

It checks for allowed numbers from 1 to 9, assign the first possible option to cell and again
call solveSudoku() method for next UNASSIGNED cell. It follows the same process recursively.

This process continues until it finds some cell for which no number is allowed and function will return false.
If function returns false, control will backtrack to previously assigned cell and try for another number there. If it finds any allowed number,
then it assigns this new number to cell and process continues again. But in case no other possible number remains,
again it will bactrack to previously assigned cell and process continues.

This process continues until it finds correct solution or reach to the stage from where no solution can be found



*/


public boolean solveSudoku()
{
    for(int row=0;row<9;row++)
    {
        for(int col=0;col<9;col++)
        {
            if(sudoku[row][col]==UNASSIGNED)
            {
                for(int number=1;number<=9;number++)
                {
                    if(isAllowed(row, col, number))
                    {
                        sudoku[row][col] = number;
                        if(solveSudoku())
                        {
                            return true;
                        }
                        else
                        {
                            sudoku[row][col] = UNASSIGNED;
                        }
                    }
                }
                return false;
            }
        }
    }
    return true;
}

// displaySudoku() method is used to display 9×9 array in form of sudoku.
public void displaySudoku()
{
    for(int i=0;i<9;i++)
    {
        if(i%3==0 && i!=0)
        {
            System.out.println("----------------------------------\n");
        }
        for(int j=0;j<9;j++)
        {
            if(j%3==0 && j!=0)
            {
                System.out.print(" | ");
            }
            System.out.print(" " + sudoku[i][j] + " ");

        }

        System.out.println();
    }
    System.out.println("\n\n__________________________________________\n\n");
