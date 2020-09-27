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


--fstChCut
--"2:xsl1:01:11:21:01:11:21:01:11:2e2:ysl1:01:01:01:11:11:11:21:21:2e2:vsl1:X1:O1:O1:X1:O1:X1:X1:X1:Oee"

-- threeBenCodesWName
--["2:xsl1:01:11:21:01:11:21:01:11:2e"
-- "2:ysl1:01:01:01:11:11:11:21:21:2e"
-- "2:vsl1:X1:O1:O1:X1:O1:X1:X1:X1:Oe"]

-- threeBenCodes
--["l1:01:11:21:01:11:21:01:11:2e"
-- "l1:01:01:01:11:11:11:21:21:2e"
-- "l1:X1:O1:O1:X1:O1:X1:X1:X1:Oe"]

-- arraysOfBenCoddedChars
--[[(1:0),(1:1),(1:2),(1:0),(1:1),(1:2),(1:0),(1:1),(1:2)]
-- [(1:0),(1:0),(1:0),(1:1),(1:1),(1:1),(1:2),(1:2),(1:2)]
-- [(1:X),(1:O),(1:O),(1:X),(1:O),(1:X),(1:X),(1:X),(1:O)]]

-- arrFromStrings
--["012012012", "000111222", "XOOXOXXXO"]

 
parse :: String -> ([Int], [Int], [Char])
parse message = 
    let
        fstChCut = L.drop 1 message
        threeBenCodesWName = splitStringInto3 fstChCut
        threeBenCodes = map removeFiveChars threeBenCodesWName
        arraysOfBenCoddedChars = map splitStringEvery3 threeBenCodes
        arrFromStrings = map benCodeArrToArr arraysOfBenCoddedChars
        xs = stringToIntArray (arrFromStrings !! 0)
        ys = stringToIntArray (arrFromStrings !! 1)
        vs = arrFromStrings !! 2

    in
        (xs, ys, vs)

stringToIntArray :: [Char] -> [Int]
stringToIntArray [] = []
stringToIntArray (h:t) = [digitToInt h] ++ stringToIntArray t

benCodeArrToArr :: [(Char,Char,Char)] -> [Char]
benCodeArrToArr [] = []
benCodeArrToArr (h:t) = [parseChar h] ++ benCodeArrToArr t

splitStringEvery3 :: [a] -> [(a,a,a)]
splitStringEvery3 ([]) = []
splitStringEvery3 (_:[]) = []
splitStringEvery3 (_:_:[]) = []
splitStringEvery3 (h1:h2:h3:t) = [(h1, h2, h3)] ++ splitStringEvery3 t

splitStringInto3 :: String -> [String]
splitStringInto3 a =
    let
        fstStr = take 33 a
        sndStr = drop 33 (take 66 a)
        trdStr = drop 66 (take 99 a)
    in
        [fstStr, sndStr, trdStr]
        

removeFiveChars :: String -> String
removeFiveChars a = L.drop 5 a

parseChar :: (Char, Char, Char) -> Char
parseChar ('1',':',a) = a
parseChar _ = error "Incorrect parameter"
