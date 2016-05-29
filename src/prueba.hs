import Data.Sequence

x= fromList [1,2,3]

f x = case viewl x of 
	1 :< xs -> 2
	2 :< xs -> 3	