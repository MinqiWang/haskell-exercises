module A4 where

import           Control.Applicative
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           A4Def
import           ParserLib

-- This can help testing by reading from a file so you can test multi-line input
-- and also have little hassle with \
parseFile :: String -> IO (Maybe Expr)
parseFile filename = do
    inp <- readFile filename
    let ans = runParser mainParser inp
    return ans

-- Parser functions
mainParser :: Parser Expr
mainParser = whitespaces *> blockPsr <* eof

blockPsr = condPsr <|> lambdaPsr <|> letPsr <|> infixPsr

condPsr = do
    keyword "if"
    ifBlock <- blockPsr
    keyword "then"
    thenBlock <- blockPsr
    keyword "else"
    elseBlock <- blockPsr
    return (Cond ifBlock thenBlock elseBlock)

reserved = ["if", "then", "else", "let", "in", "True", "False"]

lambdaPsr = do
    operator "\\"
    var <- identifier reserved
    operator "->"
    body <- blockPsr
    return (Lambda var body)

letPsr = do
    keyword "let"
    equations <- many equationPsr
    keyword "in"
    inBlock <- blockPsr
    return (Let equations inBlock)
    where equationPsr = do
            var <- identifier reserved
            operator "="
            val <- blockPsr
            char ';'
            whitespaces
            pure (var, val)

infixPsr = do
    arith <- arithPsr
    more arith
    where more arithA = do
            (do cmpop <- cmpopPsr
                arithB <- arithPsr
                return (Prim2 cmpop arithA arithB))
            <|>
            return arithA

cmpopPsr = do
    cmpop <- ((operator "==") <|> (operator "<"))
    return (case cmpop of
        "==" -> Eq
        "<" -> Lt)

arithPsr = chainl1 multiPsr (fmap Prim2 addopPsr)

multiPsr = chainl1 factorPsr (fmap Prim2 multiopPsr)

addopPsr = do
    addop <- ((operator "+") <|> (operator "-"))
    return (case addop of
        "+" -> Plus
        "-" -> Minus)

multiopPsr = do
    multiop <- ((operator "*") <|> (operator "/") <|> (operator "%"))
    return (case multiop of
        "*" -> Mul
        "/" -> Div
        "%" -> Mod)

factorPsr = do
    atoms <- some atomPsr
    return (case atoms of
        (x:xs) -> (case xs of 
            [] -> x
            _ -> foldl (App) x xs))


atomPsr = (between ((char '(') <* whitespaces) ((char ')') <* whitespaces) blockPsr) <|> literalPsr <|> varPsr

literalPsr = intPsr <|> boolPsr

intPsr = do
    int <- integer
    return (Num int)

boolPsr = do
    str <- (keyword "True") <|> (keyword "False")
    return (case str of
        "True" -> Bln True
        "False" -> Bln False)

varPsr = do
    varName <- identifier reserved
    return (Var varName)

-- Interpreter functions
mainInterp :: Expr -> Either Error Value
mainInterp = interp Map.empty

intOrDie (VN i) = pure i
intOrDie _ = Left TypeError

boolOrDie (VB i) = pure i
boolOrDie _ = Left TypeError

interp _ (Num i) = pure (VN i)

interp _ (Bln b) = pure (VB b)

interp env (Var v) = case Map.lookup v env of
    Just a -> pure a
    Nothing -> Left VarNotFound

interp env (Prim2 Plus e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (i + j))

interp env (Prim2 Minus e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (i - j))

interp env (Prim2 Mul e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (i * j))

interp env (Prim2 Div e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    case j of
        0 -> Left DivByZero
        _ -> pure (VN (div i j))

interp env (Prim2 Mod e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (mod i j))

interp env (Prim2 Eq e1 e2) = do
        a <- interp env e1
        b <- interp env e2
        case (intOrDie a) of
            Left TypeError -> case ((boolOrDie a), (boolOrDie b)) of
                                (Left TypeError, _) -> Left TypeError
                                (_, Left TypeError) -> Left TypeError
                                (Right val1, Right val2) -> pure (VB (val1 == val2))
            Right valA -> case (intOrDie b) of
                            Left TypeError -> Left TypeError
                            Right valB -> pure (VB (valA == valB))

interp env (Prim2 Lt e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VB (i < j))

interp env (Cond test eThen eElse) = do
    a <- interp env test
    case a of
      VB True -> interp env eThen
      VB False -> interp env eElse
      _ -> Left TypeError

interp env (Let eqns evalMe) = do
    env' <- extend env eqns
    interp env' evalMe
  where
    extend env [] = pure env
    extend env ((v,rhs) : eqns) = do
        a <- interp env rhs
        let env' = Map.insert v a env
        extend env' eqns

interp env (Lambda v body) = pure (VClosure env v body)

interp env (App f e) = do
    c <- interp env f
    case c of
      VClosure fEnv v body -> do
          eVal <- interp env e
          let bEnv = Map.insert v eVal fEnv
          interp bEnv body
      _ -> Left TypeError



