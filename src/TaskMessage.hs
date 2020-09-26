module Task1Message
where

-- ┌       ┐
-- │ X O O │
-- │ X O X │
-- │ X X O │
-- └       ┘
-- seed: -7894585025189160020
-- encoding: Ben
-- from: ARR
-- to: COO

size :: Int
size = 3

message :: String
message = "d2:xsl1:01:11:21:01:11:21:01:11:2e2:ysl1:01:01:01:11:11:11:21:21:2e2:vsl1:X1:O1:O1:X1:O1:X1:X1:X1:Oee"

type From = ([Int], [Int], [Char])
type To = [(Int, Int, Char)]

expectedFrom :: From
expectedFrom = ([0, 1, 2, 0, 1, 2, 0, 1, 2], [0, 0, 0, 1, 1, 1, 2, 2, 2], ['X', 'O', 'O', 'X', 'O', 'X', 'X', 'X', 'O'])

expectedTo :: To
expectedTo = [(0, 0, 'X'), (1, 0, 'O'), (2, 0, 'O'), (0, 1, 'X'), (1, 1, 'O'), (2, 1, 'X'), (0, 2, 'X'), (1, 2, 'X'), (2, 2, 'O')]
