-- returns nth L number given list of prime numbers in the increasing order in the sequence
-- *Main> nthLnumber 10 [2, 3, 5, 7]
-- 10

nthLnumber n l = if n == 1 then 1 else (nthLnumberHelper n l 1 [1])

-- numbers in the list in decreasing sequence
nthLnumberHelper n l k list_of_lnumbers = if n==k then (list_of_lnumbers!!0) else (nthLnumberHelper n l (k+1) ((generateKth l list_of_lnumbers):list_of_lnumbers))

generateKth (f:[]) list_of_lnumbers = (generateHelper f list_of_lnumbers)
generateKth (f:rest) list_of_lnumbers = min (generateHelper f list_of_lnumbers) (generateKth rest list_of_lnumbers)

generateHelper num (one:list_of_lnumbers) = (findJustBigger (one `quot` num) ((one:list_of_lnumbers)) * num)

findJustBigger num (one:[]) = one
findJustBigger num (one:two:[]) = if num >= two then one else two
findJustBigger num (one:two:list_of_lnumbers) = if num >= two then one else (findJustBigger num (two:list_of_lnumbers))

-- credits for algorithm: Piyush Kumar