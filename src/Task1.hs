import Task1Message as M
import Data.List as L
import Data.Char as C

-- parse :: Int    -- ^ Size of the matrix (number of columns or rows)
--       -> String -- ^ Encoded message
--       -> From   -- ^ Parsed data structure
-- parse _ _ = error "Not implemented"

-- convert :: Int  -- ^ Size of the matrix (number of columns or rows)
--         -> From -- ^ Parsed matrix
--         -> To   -- ^ Converted matrix
-- convert _ _ = error "Not implemented"

--message = "d2:xsl1:01:11:21:01:11:21:01:11:2e2:ysl1:01:01:01:11:11:11:21:21:2e2:vsl1:X1:O1:O1:X1:O1:X1:X1:X1:Oee"
-- expectedFrom = ([0, 1, 2, 0, 1, 2, 0, 1, 2], [0, 0, 0, 1, 1, 1, 2, 2, 2], ['X', 'O', 'O', 'X', 'O', 'X', 'X', 'X', 'O'])
--"1:01:11:21:01:11:21:01:11:2e2:ysl1:01:01:01:11:11:11:21:21:2e2:vsl1:X1:O1:O1:X1:O1:X1:X1:X1:Oee"

parse :: String -> String
parse message = removeSixChars message

-- parseString :: String -> String
-- parseString = 

removeSixChars :: String -> String
removeSixChars a = L.drop 6 a

parseChar :: String -> [Char]
parseChar ('1':':':a) = a
parseChar _ = error "Incorrect parameter"

-- tempRepeat :: Int -> String -> String
-- repeatTemp times message = 
--     let
--         [] ++ parseChar 

-- parseInt :: String -> Int
-- parseInt ('i':t) =
--     let
--         prefix = L.takeWhile C.isDigit t
--     in
--         read prefix

-- parseInt _ = error "Not an integer"
