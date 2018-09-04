module Exer67 where

type Name = String
type Price = Int
type BarCode = Int
type Database = [ (BarCode, Name, Price) ]

codeIndex:: Database
codeIndex = [ (4719, "Fish Fingers" , 121),
              (5643, "Nappies" , 1010),
              (3814, "Orange Jelly", 56),
              (1111, "Hula Hoops", 21),
              (1112, "Hula Hoops (Giant)", 133),
              (1234, "Dry Sherry, lit", 540) ]

type TillType = [BarCode]
type BillType = [(Name, Price)]

lineLength:: Int
lineLength = 30

formatPence:: Price -> String
formatPence num = inteira ++ "."  ++ fpence
                where
                inteira = show(num `div` 100)
                centavos = show(num `mod` 100)
                fpence
                  | length centavos == 1 = "0" ++ centavos
                  | otherwise = centavos


formatLine:: (Name, Price) -> String
formatLine (name, price) = name ++ (printPoints tamanho) ++ (formatPence price)
             where 
             tamanho = lineLength - (length (name) + length (formatPence price))

printPoints:: Int -> String
printPoints num 
            | num == 0 = ""
            | num > 0 = "." ++ printPoints(num-1)

formatLines:: [(Name, Price)] -> String 
formatLines [] =  ""
formatLines (x:xs) = formatLine x ++ "\n" ++  formatLines xs

--putStrLn (formatLines [(name, price) | (_,name, price) <- codeIndex]) testa 

makeTotal:: BillType -> Price
makeTotal xs = total priceList
      where
        priceList = [price | (_,price) <- xs]
        total :: [Int] -> Int
        total [] = 0
        total (x:xs) = x + total xs

formatTotal:: Price -> String
formatTotal preco = formatLine("Total", preco)
