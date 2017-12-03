import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Control.Monad.State as S
import Control.Monad.Error	
import Control.Monad.Identity
import Data.List
import System.Environment

grammar = emptyDef { identStart  = letter
               , identLetter = letter 
               , opStart     = oneOf "+-*/="
               , opLetter    = oneOf "+-*/="
               }

lexer :: TokenParser ()
lexer = makeTokenParser grammar

data Expression = Terminal String
                | Addition Expression Expression
                | Subtraction Expression Expression
                | Multiplication Expression Expression
                | Division Expression Expression
                deriving Show

parseString :: Parser Expression
parseString =  many1 letter >>= return.Terminal

parseExpression :: Parser Expression
parseExpression = (flip buildExpressionParser) parseTerm [
   [ Infix (reservedOp lexer "*" >> return Multiplication) AssocLeft
   , Infix (reservedOp lexer "/" >> return Division) AssocLeft
   ]
 , [ Infix (reservedOp lexer "+" >> return Addition) AssocLeft
   , Infix (reservedOp lexer "-" >> return Subtraction) AssocLeft 
   ]
 ]

parseTerm :: Parser Expression
parseTerm = parens lexer parseExpression <|> parseString

parseInput :: Parser Expression
parseInput = do
    whiteSpace lexer
    ex <- parseExpression
    eof
    return ex

type Evaluator a = S.StateT () (ErrorT String Identity) a

runEvaluator :: Evaluator String -> Either String (String, ())
runEvaluator calc = runIdentity $ runErrorT $ S.runStateT calc ()

eval :: Expression -> Evaluator String

eval (Terminal x) = return x

eval (Addition eLeft eRight) = do
    lft <- eval eLeft
    rgt <- eval eRight
    return $ lft ++ rgt

eval (Subtraction eLeft eRight) = do
    lft <- eval eLeft
    rgt <- eval eRight
    return $ let l = length rgt
                 n = length lft in  if (reverse $ rgt) == ((take l) (reverse lft)) then (take (n-l)) $ lft else lft

eval (Multiplication eLeft eRight) = do
    lft <- eval eLeft
    rgt <- eval eRight
    return $ let l = length rgt
                 n = length lft
                 in if l>n then concat [ (lft !! i):[(rgt !! i)]| i<- [0..n-1]] ++ ( reverse $ take (l-n) $ reverse rgt ) else  concat [ (lft !! i):[(rgt !! i)]| i<- [0..l-1]] ++ ( reverse $ take (n-l) $ reverse lft )

eval (Division eLeft eRight) = do
    lft <- eval eLeft
    rgt <- eval eRight
    return $ recursiveDel' lft rgt []

    
recursiveDel' (f_l:n_l:lft) (f_r:rgt) str = if n_l /= f_r then recursiveDel' lft rgt (f_l:n_l:str) else recursiveDel' lft rgt (f_l:str)
recursiveDel' (f_l:[]) (f_r:rgt) str = str ++ [f_l]
recursiveDel' s [] str = reverse str ++ s


calculate :: String -> String
calculate s = 
    case parse parseInput "" s of
    Left  err -> "error: " ++ (show err)
    Right exp -> case runEvaluator (eval exp) of
                 Left  err -> "error: " ++ err
                 Right (val, ()) -> show val

  
main = do
    line <- fmap head $ getArgs
    let result = calculate line
    putStrLn result
