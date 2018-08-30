module ListComp where

import Data.Char (chr,ord)
{-
 Seção: Construção de Listas por Compreensão (Intenção)
 
  Uma compreensão de listas tem a forma:
  [e | Q], onde 
    e: uma expressão
    Q: qualificador, i.e., uma sequência de geradores e guardas 
       onde
    geradores: expressões x <- xs, onde x é uma variável ou tupla
    de variáveis e xs uma expressão cujos valores são listas.
    guardas: expressões booleanas
    
  
-}

 

lc01 = [x*x | x<-[1..5],odd x]
{-
x     = 1   2   3   4  5 
odd x = T   F   T   F  T
x*x  =  1   -   9   -  25
-}
bool = [True,False]
lco02 = [(x,y) | x<-bool,y<-bool]
{-
Avaliação:

[(True,y) | y <- bool] ++ [(False,y)| y <- bool]
=[(True,True)] ++ [(True,False)] ++ [(False,True)] ++ [(False,False)]
= ...
-}

exlist = [2,4,7]
lc02 = [2*n | n <- exlist]
lc03 = [even n | n <- exlist]
lc04 = [2*n| n <- exlist, even n, n>3]
pairList = [(2,3),(2,1),(7,8)]
lc05 = [m+n | (m,n) <- pairList]
addPairs xsPairs = [m+n | (m,n) <- xsPairs]
addOrdPairs xsPairs = [m+n | (m,n) <- xsPairs, m < n]

{- 
Define a function isDigit::Char -> Bool and then test the following definition:

digits str = [ ch | ch <- str, isDigit str]
-}

isDigit::Char -> Bool
isDigit c = c `elem` ['0'..'9']

digits str = [c | c <- str, isDigit c]

allEven  xs = (xs == [x | x<- xs, even x])

{- Exercícios 5.18-5.26, Haskell Craft -}

divisors:: Integer -> [Integer]
divisors m = [x | x <- [1..m], m `mod` x == 0]

isPrime:: Integer -> Bool
isPrime m = divisors m == [1,m]

matches:: Integer -> [Integer] -> [Integer]
matches m x = [y | y <- x, m `notElem` x]



{- Estudar seção 5.7 (Haskell Craft) e depois fazer
   Exercícios 5.27-5.34 -}

-- Section: Basic Logic

infixr 1  ==> -- proridade:1-->9, infix rightassociative

(==>)::Bool->Bool->Bool
True ==> b = b
False ==> b = True

infixr 1 <==>
(<==>)::Bool->Bool->Bool
True <==> b = b
False <==> b = not b
form01::Bool -> Bool -> Bool
form01 p q =  (not p) && (p==>q) <==> not(q && not p)

form02:: Bool -> Bool -> Bool
form02 p q = (p ==> q) <==> (not(p) || q)

{- Exercício: Com base no tipo da fórmula, explique a diferença
   essencial formula1 e formula2.
-}

valid1:: (Bool->Bool)->Bool
valid1 bf = (bf True) && (bf False)

excluded_middle:: Bool -> Bool
excluded_middle p =  p || not p

valid01 = valid1 excluded_middle 

{-
  -- Exercícios sobre Lógica Proposicional
  
  Exercício: Utilizando o construtor de listas por compreensão 
  definir funções valid2 e valid3 que verificam se expressões
  booleanas de 2 e 3 variáveis são válidas.
  
  Exercício: Utilizando o construtor de listas por compreensão 
  definir funções valid2 e valid3 que verificam se expressões
  booleanas de 2 e 3 variáveis são contradições.
  
  Exercício: Utilizando o construtor de listas por compreensão 
  definir funções valid2 e valid3 que verificam se expressões
  booleanas de 2 e 3 variáveis são satisfatíveis.
  
  Exercício: Utilizando o construtor de listas por compreensão 
  definir definir uma função que computa a lista de modelos 
  de uma lista de fórmulas de duas variáveis. Um modelo de uma
  lista de fórmulas é uma valoração para as variáveis que satisfaz
  todas as fórmulas da lista.  
  
-}
