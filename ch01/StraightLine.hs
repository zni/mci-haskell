module StraightLine where

type Id = String

data BinOp = Plus | Minus | Times | Div
    deriving (Show)

data Stm =
    CompoundStm Stm Stm |
    AssignStm Id Exp    |
    PrintStm [Exp]
    deriving (Show)

data Exp =
    IdExp Id            |
    NumExp Integer      |
    OpExp Exp BinOp Exp |
    EseqExp Stm Exp
    deriving (Show)

prog = CompoundStm (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
                   (CompoundStm (AssignStm "b" (EseqExp (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
                                                        (OpExp (NumExp 10) Times (IdExp "a"))))
                                (PrintStm [IdExp "b"]))

prog2 = CompoundStm (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
                    (PrintStm [IdExp "a", IdExp "a", EseqExp (PrintStm [IdExp "a", IdExp "a", IdExp "a"]) (IdExp "a")])

maxargs :: Stm -> Int
maxargs (CompoundStm left right) =
    let left' = maxargs left
        right' = maxargs right
    in if left' < right'
            then right'
            else left'
maxargs (AssignStm id exp) = maxargsExp exp
maxargs (PrintStm exp) =
    let total = length exp
        exp'  = maximum . map maxargsExp $ exp
    in if total < exp'
            then exp'
            else total

maxargsExp :: Exp -> Int
maxargsExp (IdExp _) = 0
maxargsExp (NumExp _) = 0
maxargsExp (OpExp left op right) =
    let left' = maxargsExp left
        right' = maxargsExp right
    in  if left' < right'
            then right'
            else left'

maxargsExp (EseqExp stm exp) =
    let stm' = maxargs stm
        exp' = maxargsExp exp
    in if stm' < exp'
            then exp'
            else stm'


type Table = [(Id, Integer)]

interp :: Stm -> IO ()
interp (CompoundStm left right) = do
    table' <- interpStm left []
    table''<- interpStm right table'
    return ()
interp a@(AssignStm _ _) = do
    interpStm a []
    return ()
interp p@(PrintStm _) = do
    interpStm p []
    return ()

interpStm :: Stm -> Table -> IO Table
interpStm (CompoundStm left right) table = do
    table' <- interpStm left table
    table'' <- interpStm right table'
    return table''

interpStm (AssignStm id exp) table = do
    (n, table') <- interpExp exp table
    return $ update id n table'

interpStm (PrintStm exps) table = do
    tables <- mapM (flip interpExp table) exps
    let (_, table') = last tables
    mapM_ (\(x, y) -> print x) tables
    return table'

interpExp :: Exp -> Table -> IO (Integer, Table)
interpExp (IdExp id) table = return (StraightLine.lookup id table, table)

interpExp (NumExp n) table = return (n, table)

interpExp (OpExp left Plus right) table = do
    (n, table') <- interpExp left table
    (m, table'') <- interpExp right table'
    return (n + m, table'')

interpExp (OpExp left Minus right) table = do
    (n, table') <- interpExp left table
    (m, table'') <- interpExp right table'
    return (n - m, table'')

interpExp (OpExp left Times right) table = do
    (n, table') <- interpExp left table
    (m, table'') <- interpExp right table'
    return (n * m, table'')

interpExp (OpExp left Div right) table = do
    (n, table') <- interpExp left table
    (m, table'') <- interpExp right table'
    return (n `div` m, table'')

interpExp (EseqExp stm exp) table = do
    table' <- interpStm stm table
    interpExp exp table'

update :: Id -> Integer -> Table -> Table
update i n t = (i, n):t

lookup :: Id -> Table -> Integer
lookup _ [] = error "undefined variable"
lookup id ((var, n):xs) =
    if id == var
        then n
        else StraightLine.lookup id xs

