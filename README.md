# Fitness trackers

## The first stage
### Task1
I traverse the matrix and with the help of a local function, I process each row from 
the table. I concatenate the name at the beginning, and I traverse the number of steps 
with a foldr and an anonymous function, adding up all the steps and storing them in an 
accumulator. I divide by 8 to find the average. The result is converted to a string.

### Task2

- get_passed_people_num

  I use the function from task 1 to calculate the sum of the steps. Then, with a 
foldr, I traverse the matrix accessing the numbers from index 1, which is the sum 
of the steps. With an if statement, I check if it has at least 1000 steps, and if 
it returns true, I add 1 to the accumulator.
- get_steps_avg

   With foldr, I traverse the elements of the matrix without the header resulted 
from task 1. I access the average steps with "!! 1", multiply it by 8 because I 
need the total number and add it to the accumulator. I divide the result returned 
by foldr by the total number of people to find the average.

### Task3
I apply foldr to the tail of the transposed matrix. I transpose the matrix to make 
it easier to access the steps of everyone at each hour. The function 'op' is applied
to the tail of 'xs' because I don't need the names of the people. The function takes
each element, casts it to float, and divides the result by the total number of people.

### Task 4
- my_range

   The function takes a row and returns a row consisting of 3 accumulators. 'acc1' 
represents the minutes between 0 and 50, 'acc2' represents the minutes between 50 
and 100, and 'acc3' represents the minutes between 100 and 500. The function is 
constructed as follows: I take one element from the list, cast it to int, and using
'guards', I check the 3 cases. At the end, I convert the accumulators to strings 
and put them in an array.
- get_activ_summary
  
   I transpose the matrix and with the help of 'drop', I extract the last 3 rows. 
With a foldr, I go through them and call the 'my_range' function.
### Task 6

- get_steps

  I receive a 'Row' and an accumulator [Row] in which I will return the output. 
The name will be on the first position of the array (head xs), on the second position
will be the average of the steps for the first 4 days, on the third position will be
the average of the steps for the last 4 days. I obtain the first 4 days with 'take' 
and the last 4 days with 'drop' and call the 'average_of_steps' function on them. On
the fourth position, I will put the difference that I obtained with the 'most_productive_days'
function.

## The second stage

### Task 3
The 'equal_matrix' function fills the smaller matrix with empty strings. For this, I
use the 'take' and 'repeat' functions. 'take m(repeat(take n(repeat "")))' creates 
'm' rows with 'n' empty strings each, which I add to the shorter matrix. To 
concatenate row by row, I check which matrix is shorter, apply the above function 
on it, and use 'zipWith' to concatenate the rows.

### Task 5
To create the Cartesian product, I used 2 nested 'foldr' functions. The first 'foldr'
iterates over the first matrix, and in the local function of this 'foldr', I used 
another function where I iterate over the second matrix. The function takes a row 
from the first matrix and another from the second matrix as a parameter. I iterate 
over the second matrix and call the function passed as a parameter between the row 
from the first matrix and all the rows from the second matrix, adding the result to
the accumulator which I return. Then the first 'foldr' takes the next row from the
first matrix and so on. (Equivalent to 2 nested 'for' loops).

### Task 6
I iterate through the column names array using foldr, and through the transposed 
matrix using another foldr, to be able to access the columns more easily. I compare
each column name with each column in the matrix using an if statement, and if they 
are equal, I add the column to the accumulator. Finally, I transpose the result 
returned by foldr so that the matrix is in its initial shape.