import Data.List
-- import Task1Message

-- parse :: Int    -- ^ Size of the matrix (number of columns or rows)
--       -> String -- ^ Encoded message
--       -> From   -- ^ Parsed data structure
-- parse _ _ = error "Not implemented"

-- convert :: Int  -- ^ Size of the matrix (number of columns or rows)
--         -> From -- ^ Parsed matrix
--         -> To   -- ^ Converted matrix
-- convert _ _ = error "Not implemented"

-- parse :: [Char] -> [Char]
-- parse message = message


-- triples :: [a] -> [(a,a,a)]
-- triples [] = []
-- triples (_:[]) = []
-- triples (_:_:[]) = []
-- triples (h1:h2:h3:t) = [(h1, h2, h3)] ++ triples t


-- addArrayUp :: [Integer] -> Integer
-- addArrayUp [] = 0
-- addArrayUp (h:t) = h + addArrayUp t


-- test :: Integer -> Integer
-- test a = a + 5

-- addOne :: Integer -> Integer
-- addOne a = a + 1\

--------BAD---------------
-- test2 :: [Char] -> [Char]
-- test2 = splitAt 3 "mimimimi"

parseInt :: String -> Int
parseInt ('i':t)
	let
		prefix = L.takeWhile C.isDigit t
	in
		read prefix

parseInt _ - error "Not an integer"