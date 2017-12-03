import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Data.Map as M
import qualified Control.Monad.State as S
import Control.Monad.Error	
import Control.Monad.Identity
import Data.List
import System.Environment


def = emptyDef { identStart  = letter
               , identLetter = letter 
               , opStart     = oneOf "+-*/="
               , opLetter    = oneOf "+-*/="
               }

lexer :: TokenParser ()
lexer = makeTokenParser def

data Expression = Terminal String
                | Addition Expression Expression
                | Subtraction Expression Expression
                | Multiplication Expression Expression
                | Division Expression Expression
                deriving Show

parseString :: Parser Expression
parseString = do
    v <- many1 letter
    return $ Terminal $ v

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
parseTerm = parens lexer parseExpression 
        <|> parseString

parseInput :: Parser Expression
parseInput = do
    whiteSpace lexer
    ex <- parseExpression
    eof
    return ex


type SymTab = M.Map String String

type Evaluator a = S.StateT SymTab (ErrorT String Identity) a

runEvaluator :: Evaluator String -> SymTab -> Either String (String, SymTab)
runEvaluator calc symTab = runIdentity $ runErrorT $ S.runStateT calc symTab

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


defaultVars :: M.Map String String
defaultVars = M.empty

calculate :: SymTab -> String -> (String, SymTab)
calculate symTab s = 
    case parse parseInput "" s of
    Left  err -> ("error: " ++ (show err), symTab)
    Right exp -> case runEvaluator (eval exp) symTab of
                 Left  err              -> ("error: " ++ err, symTab)
                 Right (val, newSymTab) -> (show val, newSymTab)

  
main = do
    line <- fmap head $ getArgs
    let (result, _) = calculate defaultVars line
    putStrLn result