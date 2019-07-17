module HexString

hexVal : Char -> Maybe Int
hexVal '0' = Just 0
hexVal '1' = Just 1
hexVal '2' = Just 2
hexVal '3' = Just 3
hexVal '4' = Just 4
hexVal '5' = Just 5
hexVal '6' = Just 6
hexVal '7' = Just 7
hexVal '8' = Just 8
hexVal '9' = Just 9
hexVal 'a' = Just 10
hexVal 'b' = Just 11
hexVal 'c' = Just 12
hexVal 'd' = Just 13
hexVal 'e' = Just 14
hexVal 'f' = Just 15
hexVal 'A' = Just 10
hexVal 'B' = Just 11
hexVal 'C' = Just 12
hexVal 'D' = Just 13
hexVal 'E' = Just 14
hexVal 'F' = Just 15
hexVal c = Nothing

valHex : Integer -> Char
valHex 0 = '0'
valHex 1 = '1'
valHex 2 = '2'
valHex 3 = '3'
valHex 4 = '4'
valHex 5 = '5'
valHex 6 = '6'
valHex 7 = '7'
valHex 8 = '8'
valHex 9 = '9'
valHex 10 = 'a'
valHex 11 = 'b'
valHex 12 = 'c'
valHex 13 = 'd'
valHex 14 = 'e'
valHex 15 = 'f'
valHex c = '?'

export
parseHex : List Char -> Int -> Maybe Int
parseHex []        acc = Just acc
parseHex (c :: cs) acc =
  case hexVal c of
      (Just v) => parseHex cs ((acc * 16) + v)
      Nothing => Nothing


printHex : Integer -> String
printHex 0 = ""
printHex x = printHex (x `div` 16) ++ singleton (valHex (x `mod` 16))

export
printHex6 : Integer -> Maybe String
printHex6 x =
    let hex = printHex x in
    if length hex > 6 then
        Nothing
    else
        let toPad = 6 `minus` length hex in
        Just $ pack (take toPad (repeat '0')) ++ hex
