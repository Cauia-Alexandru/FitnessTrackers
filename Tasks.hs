
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array

import Common

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1
compute_average_steps :: Table -> Table
compute_average_steps m = ["Name","Average Number of Steps"] : (map op (tail m))
                          where
                                op [] = []
                                op xs = head xs : [((printf "%.2f") ((fromIntegral (foldr (\x acc -> (read x) + acc ) 0 (tail xs)) :: Float) / (8)))]


-- Task 2


-- Number of people who have achieved their goal:
get_passed_people_num :: Table -> Int
get_passed_people_num m = foldr op 0 (tail (compute_average_steps m))
                          where
                               op :: Row -> Int -> Int
                               op [] acc = acc
                               op xs acc = acc + if (((read (xs !! 1) :: Float) * 8)) >= 1000 then 1 else 0
                          


nrOfPeople :: Table -> Int
nrOfPeople m = length (tail m)

get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = ((fromIntegral (get_passed_people_num m)) / (fromIntegral (nrOfPeople m)))



get_steps_avg :: Table -> Float
get_steps_avg m = (foldr op 0 (tail(compute_average_steps m))) / fromIntegral(nrOfPeople m)
                  where
                       op :: Row -> Float -> Float
                       op [] acc = acc
                       op xs acc = acc + ((read (xs !! 1) :: Float) * 8)


-- Task 3

tr :: Table -> Table
tr ([]:_) = []
tr m = (map head m) : (tr(map tail m))


get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = ["H10","H11","H12","H13","H14","H15","H16","H17"] : (foldr op [] (tail (tr m))) : []
                        where
                             op xs acc = ((printf "%.2f") ((fromIntegral (foldr (\x acc -> (read x) + acc ) 0 (tail xs)) :: Float) / (fromIntegral(nrOfPeople m)))) : acc
                             

-- Task 4


my_range :: Row -> Int -> Int -> Int -> Row
my_range [] acc1 acc2 acc3 = show acc1 : show acc2 : show acc3:[]
my_range (x:xs) acc1 acc2 acc3
    | (read x) >= 0 && (read x) < 50 = my_range xs (acc1 + 1) acc2 acc3
    | (read x) >= 50 && (read x) < 100 = my_range xs acc1 (acc2 + 1) acc3
    | (read x) >= 100 && (read x) < 500 = my_range xs acc1 acc2 (acc3 + 1)


get_activ_summary :: Table -> Table
get_activ_summary m = ["column","range1","range2","range3"] : (foldr op [] (drop 3 (tr m))) 
                      where
                           op :: Row -> [Row] -> [Row]
                           op xs acc = (head xs : my_range (tail xs) 0 0 0) : acc


-- Task 5



compare_function a b     | (read (a !! 1) :: Int) < (read (b !! 1) :: Int) = LT
                         | (read (a !! 1) :: Int) > (read (b !! 1) :: Int) = GT
                         | otherwise = compare (a !! 0) (b !! 0)

get_ranking :: Table -> Table
get_ranking m = ["Name","Total Steps"] : (sortBy compare_function (tr(take 2 (tr (tail m)))))



-- Task 6

average_of_steps :: Row -> Float
average_of_steps xs = ((fromIntegral (foldr (\x acc -> (read x) + acc ) 0 (xs)) :: Float) / (4))


most_productiv_days :: Float -> Float -> Float
most_productiv_days x y 
                   | x >= y = x - y
                   | y > x = y - x



get_steps :: Row -> [Row] -> [Row]
get_steps xs acc = [head xs, ((printf "%.2f")(average_of_steps (take 4 (tail xs)))), ((printf "%.2f")(average_of_steps (drop 4 (tail xs)))), 
            ((printf "%.2f")(most_productiv_days (average_of_steps (take 4 (tail xs))) (average_of_steps (drop 4 (tail xs)))))] : acc


--Apelez functia get_steps pe toata matricea cu un foldr
get_steps_diff_table1 :: Table -> Table
get_steps_diff_table1 m = (foldr get_steps [] (tail m))
                         

compare_function_6 :: Row -> Row -> Ordering
compare_function_6 a b  | (read (a !! 3) :: Float) < (read (b !! 3) :: Float) = LT
                        | (read (a !! 3) :: Float) > (read (b !! 3) :: Float) = GT
                        | otherwise = compare (a !! 0) (b !! 0)

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = ["Name","Average first 4h","Average last 4h","Difference"] : (sortBy compare_function_6) (get_steps_diff_table1 m)
                             

-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map (map f) m


-- Task 8

-- Applies the given function to all the entries

rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : (map f (tail m))

--Functia calculeaza totalul minutelor dormite pe toata saptamana
sum_sleep :: Row -> Float -> Float
sum_sleep [] acc = acc
sum_sleep (x:xs) acc = sum_sleep xs (read x + acc) 

--Concatenez emailul cu totalul minutelor dormite
get_sleep_total :: Row -> Row
get_sleep_total r = head r : (((printf "%.2f")(sum_sleep (tail r) 0)) : []) 


{-
    TASK SET 2
-}

-- Task 1
my_index :: Row -> ColumnName -> Int -> Int
my_index [] name acc = -1
my_index (x:xs) name acc  | x == name = acc
                          | otherwise = my_index xs name (acc + 1)

isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

tsort :: ColumnName -> Table -> Table
tsort column table = head table : if (isNumber((head(tail table)) !! (my_index (head table) column 0)) == True)
                        then sortBy compare_numbers (tail table) else sortBy compare_by_strings (tail table)
                            where   compare_numbers a b     | (read (a !! (my_index (head table) column 0)) :: Int) < (read (b !! (my_index (head table) column 0)) :: Int) = LT
                                                            | (read (a !! (my_index (head table) column 0)) :: Int) > (read (b !! (my_index (head table) column 0)) :: Int) = GT
                                                            | otherwise = compare (a !! 0) (b !! 0)
                                    compare_by_strings a b  | (a !! (my_index (head table) column 0)) < (b !! (my_index (head table) column 0)) = LT
                                                            | (a !! (my_index (head table) column 0)) > (b !! (my_index (head table) column 0)) = GT
                                                            | otherwise = compare (a !! 0) (b !! 0)


-- Task 2

vunion :: Table -> Table -> Table
vunion t1 t2 = if(head t1 == head t2) then (t1 ++ (tail t2)) else t1
        

-- Task 3 
--daca t1 e mai mica returnez t1 implinita cu stringuri goale
equal_matrix :: Table -> Table -> Table
equal_matrix t1 t2 = t1 ++ (take (length t2 - length t1)(repeat (take (length (head t1))(repeat ""))))
    
hunion :: Table -> Table -> Table
hunion t1 t2  | length t1 < length t2 = zipWith (++) (equal_matrix t1 t2) t2
              | length t2 < length t1 = zipWith (++) t1 (equal_matrix t2 t1)
              | otherwise = zipWith (++) t1 t2

-- Task 4
tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = [[]]

-- Task 5

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names :  (foldr op [] (tail t1))
    where
        op :: Row -> Table -> Table
        op r1 acc = (row2 new_row_function r1 t2) ++ acc

row2 :: (Row -> Row -> Row) -> Row -> Table -> Table
row2 new_row_function r1 t2 = foldr op [] (tail t2)
    where
        op r2 acc = (new_row_function r1 r2) : acc
-- Task 6

projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t = tr (foldr op [] columns_to_extract)
    where
        op name acc = (extract name t) ++ acc

extract :: String -> Table -> Table
extract name t = foldr op [] (tr t)
    where
        op :: Row -> Table -> Table
        op r acc = if(name == (head r)) then (r : acc) else acc

-- Task 7
-- my_index :: Row -> ColumnName -> Int -> Int
-- my_index [] name acc = -1
-- my_index (x:xs) name acc  | x == name = acc
--                           | otherwise = my_index xs name (acc + 1)


filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = (head t) : foldr op [] (tail t)
    where
        op :: Row -> Table -> Table
        op xs acc = if (condition (xs !! (my_index (head t) key_column 0)) == True) then xs : acc else acc

-- Task 8 TO_DO


{-
    TASK SET 3
-}


-- 3.1

data Query =
    FromTable Table
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query -- 3.4
    | Graph EdgeOp Query -- 3.5

instance Show QResult where
    show (List l) = show l
    show (Table t) = show t

class Eval a where
    eval :: a -> QResult
 
process_table :: QResult -> Table
process_table (Table table) = table

instance Eval Query where
    eval (FromTable table) = Table table
    eval (AsList colname table) = List ((tr (tail (process_table (eval table)))) !! (my_index (head (process_table (eval table))) colname 0))
    eval (Sort colname  table) = Table (tsort colname (process_table (eval table)))
    eval (ValueMap op  table) = Table (vmap op (process_table (eval table)))
    eval (RowMap op colnames  table) = Table (rmap op colnames (process_table (eval table)))
    eval (VUnion  t1  t2) = Table (vunion (process_table (eval t1)) (process_table (eval t2)))
    eval (HUnion  t1  t2) = Table (hunion (process_table (eval t1)) (process_table (eval t2)))
    eval (TableJoin colname  t1 t2) = Table (tjoin colname (process_table (eval t1)) (process_table (eval t2)))
    eval (Cartesian op colnames t1 t2) = Table (cartesian op colnames (process_table (eval t1)) (process_table (eval t2)))
    eval (Projection colnames table) = Table (projection colnames (process_table (eval table)))
    eval (Filter condition table) = 
        Table ((head (process_table (eval table))) : foldr (\row acc -> if (feval (head (process_table (eval table))) condition row == True) then row : acc else acc) [] (tail (process_table (eval table))))

-- 3.2 & 3.3

type FilterOp = Row -> Bool

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

in_list :: [Float] -> Float -> Bool
in_list [] ref = False
in_list (x:xs) ref = if (x == ref) then True else (in_list xs ref)

in_list_strings :: [String] -> String -> Bool
in_list_strings [] ref = False
in_list_strings (x:xs) ref = if (x == ref) then True else (in_list_strings xs ref)

instance FEval Float where
    feval col_names (Eq colname ref) = (\row -> (read (row !! (my_index col_names colname 0)) :: Float) == ref) 
    feval col_names (Lt colname ref) = (\row -> (read (row !! (my_index col_names colname 0)) :: Float) < ref)
    feval col_names (Gt colname ref) = (\row -> (read (row !! (my_index col_names colname 0)) :: Float) > ref)
    feval col_names (In colname list) = (\row -> in_list list (read (row !! (my_index col_names colname 0)) :: Float)) 
    feval col_names (FNot cond) = (\row -> not (feval col_names cond row))
    feval col_names (FieldEq c1 c2) = (\row -> (read (row !! (my_index col_names c1 0)) :: Float) == (read (row !! (my_index col_names c2 0)) :: Float))
  
instance FEval String where
    feval col_names (Eq colname ref) = (\row -> (row !! (my_index col_names colname 0)) == ref)
    feval col_names (Lt colname ref) = (\row -> (row !! (my_index col_names colname 0)) < ref)
    feval col_names (Gt colname ref) = (\row -> (row !! (my_index col_names colname 0)) > ref)
    feval col_names (In colname list) = (\row -> in_list_strings list (row !! (my_index col_names colname 0)))
    feval col_names (FNot cond) = (\row -> not (feval col_names cond row))
    feval col_names (FieldEq c1 c2) = (\row ->(row !! (my_index col_names c1 0)) == (row !! (my_index col_names c2 0)))

-- 3.4

-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

-- 3.5
similarities_query :: Query
similarities_query = undefined

-- 3.6 (Typos)
correct_table :: String -> Table -> Table -> Table
correct_table col csv1 csv2 = undefined
