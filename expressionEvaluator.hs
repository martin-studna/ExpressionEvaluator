import Data.List
import Data.Char
import System.IO
import Control.Monad
{- Martin Studna - Zápočtový program "Analýza infixní notace" -}


{- Tokenize rozdělí vstupní řetězec na seznam řetězců -}
tokenize :: String -> [String]
tokenize [] = []
tokenize (x : xs)                 
    | isDigit x = let (d,ys) = span isDigit (x : xs) in d : tokenize ys
    | isSpace x = tokenize xs 
    | otherwise = [x] : tokenize xs

{- Reverse seznamu řetězců -}
reverseList :: [String] -> [String]
reverseList [] = []
reverseList (x:xs) =  (reverseList xs) ++ [x]

{- Funkce infixToPostfix implementována podle Shunting yard algoritmu.
   Převede aritmetický výraz (v infixové notaci) na zápis v postfixu.
   Na vstupu je seznam řetězců, zásobník a výstupní seznam.
-}
infixToPostfix :: [String] -> [String] -> [String] -> [String]
infixToPostfix output operators [] = (reverse output) ++ operators          {- Poslední část algoritmu: Pokud zbyly operátory na zásobníku, dají se na výstup -}
infixToPostfix output [] (x:xs)                                             {- 1. případ (Inicializace): zásobník je prázdný -}
    | x `elem` ["*","-","+","(","/","^"] = infixToPostfix output [x] xs     {- Na vstupu je operátor, přidat na zásobník -}
    | x == ")" = error "mismatched parenthesis"                             
    | otherwise = infixToPostfix (x:output) [] xs                           {- Jinak se uloží čísla a znaky na výstup -}
infixToPostfix output (op:ops) (x:xs)                                       
    | x == "(" = infixToPostfix output (x:op:ops) xs                        {- Levou závorku přidej na zásobník -}
    | x == ")" = case (span (/= "(") (op:ops)) of                           {- Na vstupu je pravá závorka. Dokud není nalezena levá závorka na zásobníku, tak -}                   
        (as, b:bs) -> infixToPostfix (as ++ output) bs xs                   {- se berou operatory ze zásobníku na výstup. -}
    | x `elem` ["^"] = infixToPostfix output (x:op:ops) xs                  {- Mocnění má nejvyší prioritu -}
    | x `elem` ["*"] =   case (op) of                                       {- na vstupu "*" 2. případy -}
        "/" -> infixToPostfix (op:output) (x:ops) xs                        {- Dělení má stejnou prioritu jako násobení, dávám na výstup. -}
        "^" -> infixToPostfix (ops ++ op:output) [x] xs                     {- Dám všechno ze zásobníku na výstup a uložím "^".-}
        otherwise -> infixToPostfix output (x:op:ops) xs                    {- Čísla a znaky na výstup -}
    | x `elem` ["/"] =   case (op) of                                       {- Analogicky jako u násobení -}
        "*" -> infixToPostfix (op:output) (x:ops) xs
        "^" -> infixToPostfix (ops ++ op:output) [x] xs
        otherwise -> infixToPostfix output (x:op:ops) xs
    | x `elem` ["+","-"] = case (op) of                                  {- "+" a "-" mají nejnižší prioritu. Podobné jako v předchozím případě -}
        "*" -> infixToPostfix (op:output) (x:ops) xs
        "/" -> infixToPostfix (op:output) (x:ops) xs
        "^" -> infixToPostfix (ops ++ op:output) [x] xs
        "(" -> infixToPostfix output (x:op:ops) xs
        otherwise -> infixToPostfix (op:output) (x:ops) xs
    | otherwise = infixToPostfix (x:output) (op:ops) xs

{- toPostfix používá předchozí funkce, akorát na vstupu a výstupu je řetězec -}
toPostfix :: String -> String                                           
toPostfix =  intercalate " " . infixToPostfix [] [] . tokenize      
{- tokenize převede řetězec na seznam řetězců a intercalate převede seznam zase na řetězec. -}

{- Převod výrazu na syntaktický strom. Nejdřív se převede řetězec na postfix a pak se uskuteční převod. -}
parse :: String -> Tree
parse input = add Empty (tokenize (toPostfix input)) [] 

{- Expr String ~ Leaf String -}
data Tree = Expr String | Empty | Node Tree String Tree deriving (Show)

{- add přidává "tokeny" seznamu do stromu. Postup je podobný jako u minulého algoritmu.
   Na vstupu je prázdný strom, seznam řetězců (vstup - postfixový výraz) a zásobník podstromů (vrcholů).
   Pokud je na vstupu operand, tak se vždycky "pushuje" na zásobník.
   Pokud je na vstupu operátor, tak se vytvoří pro něj vrchol, "popnou" se vrchní dva prvky zásobníku, kteří budou
   potomci nového vrcholu a operator se uloží na zásobník. -}

add :: Tree -> [String] -> [Tree] -> Tree
add Empty [] [] = Empty             
add Empty (x:xs) [] = add Empty xs [Expr x]     {- Inicializace: na začátku vstupu je vždycky operand a ten se uloží na zásobník. -}                
add Empty (x:xs) [a] = add Empty xs (Expr x:[a])
add Empty (x:xs) (a:b:bs)
    | x `elem` ["*","-","+","/","^"] = add (Node b x a) xs ([Node b x a] ++ bs)
    | otherwise = add Empty xs ([Expr x] ++ (a:b:bs))
add (Node l v p) [] [] = Node l v p
add (Node l v p) [] stack = Node l v p
add (Node l v p) (x:xs) [a] = add (Node l v p) xs (Expr x:[a])
add (Node l v p) (x:xs) (a:b:bs)
    | x `elem` ["*","-","+","/","^"] = add (Node b x a) xs ([Node b x a] ++ bs)
    | otherwise = add (Node l v p) xs (Expr x:a:b:bs)

{- Pro převod výrazu do prefixové notace se využije "preorder" procházení stromu -}
toPrefix :: String -> String
toPrefix input = intercalate " " (treeToPreorder (parse input))

{-Procházení binárního stromu - Preorder -}
treeToPreorder :: Tree -> [String]
treeToPreorder = preorder
    where preorder Empty = []
          preorder (Expr x) = [x]
          preorder (Node l v p) = v : preorder l ++ preorder p



{- Funkce dostane aritmetický výraz, vybere se proměnná, za kterou se má aplikovat substituce a
   nakonec hodnota, která se má dosadit-}
substitution :: [String] -> [String] -> String -> String -> [String]
substitution [] output a b = reverseList output
substitution (x:xs) [] a b
    | x == a = substitution xs [b] a b 
    | otherwise = substitution xs [x] a b
substitution (x:xs) output a b
    | x == a = substitution xs ([b] ++ output) a b
    | otherwise = substitution xs ([x] ++ output) a b 

sub :: String -> String -> String -> String
sub input x y = intercalate " " (substitution (tokenize input) [] x y)

{- Evaluace postfixní notace -}
solvePostfix :: String -> Float  
solvePostfix = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys 
            foldingFunction (x:y:ys) "^" = (x ** y):ys 
            foldingFunction xs numberString = read numberString:xs

{- Evaluace prefixní notace -}
solvePrefix :: String -> Float
solvePrefix input = solvePostfix (intercalate " " (reverseList (tokenize input)))

{- Uživatelské rozhraní programu-}
main = do
    putStrLn "Enter expression:"
    expression <- getLine


    putStrLn "Enter a number from 1 to 5:"
    putStrLn "1. Convert to Postfix"
    putStrLn "2. Convert to Prefix"
    putStrLn "3. Convert to Binary Expression Tree"
    putStrLn "4. Substitution"
    putStrLn "5. Evaluate Expression"
    choice <- getLine

    case (choice) of
        "1" -> putStrLn $ toPostfix expression
        "2" -> putStrLn $ toPrefix expression
        "3" -> print (parse expression)
        "4" -> do { putStrLn "Enter a variable: "
                    ; variable <- getLine
                    ; putStrLn "Enter a number: " 
                    ; number <- getLine
                    ; print (sub expression variable number)
                    ; print (solvePostfix (toPostfix (sub expression variable number))) }
        "5" -> print (solvePostfix (toPostfix expression))
        otherwise -> putStrLn "This is not a number from 1 to 5!"

