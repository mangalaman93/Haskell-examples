-- coinChange: function to calculate number of possible
--				combination of coins to get value x
-- *Main> coinChange 111 [5, 7, 56]
-- 5
--
-- minCoinChange: gives the minimum possible coin combination
-- 				  return type: [(number of coins, denomination)]
-- *Main> minCoinChange 111 [5, 7, 56]
-- [(4,5),(5,7),(1,56),(0,0)]

coinChange 0 _ = 1
coinChange  _ [] = 0
coinChange x l = sum [coinChange (x - (n * (head l))) (tail l) | n <- [0..(x `quot` (head l))]]

coinChangeMin 0 _ = [(0, 0)]
coinChangeMin  _ [] = [(-1, 0)]
coinChangeMin x l = myMin [((n, (head l)) : coinChangeMin (x - (n * (head l))) (tail l)) | n <- [0..(x `quot` (head l))]]

myMin (one:[]) = one
myMin (one:list_of_list_of_tuples) =
	let two = (myMin list_of_list_of_tuples)
	in (
		if ((mySum one) < 0)
		then two
		else (
			if((mySum two) < 0)
			then one
			else (
				if (mySum one) < (mySum two)
			  	then one
			  	else two
			)
		)
	)

mySum (tuple:[]) = fst tuple
mySum (tuple:list_of_tuples) = 
	let med_sum = (mySum list_of_tuples)
	in (
		if (med_sum < 0 || (fst tuple) < 0)
		then -1
		else ((fst tuple) + med_sum)
	)

minCoinChange x l =
	let temp = (coinChangeMin x l)
	in (
		if (mySum temp) < 0
		then (error "Not possible to find min coins")
		else temp
	)