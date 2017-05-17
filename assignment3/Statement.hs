module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip |
    Begin [Statement] |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Comment String
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment variable expression: stmts) dict input =
    let dictionary = Dictionary.insert (variable, Expr.value expression dict) dict
    in exec stmts dictionary input
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Skip: stmts) dict input =
    exec stmts dict input
exec (Begin beginStmts: stmts) dict input =
    exec (beginStmts++stmts) dict input
exec (While cond whileStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (whileStmts: While cond whileStmts: stmts) dict input
    else exec stmts dict input
exec (Read variable: stmts) dict (h:input) =
    let dictionary = Dictionary.insert (variable, h) dict
    in exec stmts dictionary input
exec (Write expression: stmts) dict input =
    (Expr.value expression dict):(exec stmts dict input)
--exec (Comment str: stmts) dict input

skipStatement = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip

ifStatement = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf
buildIf ((e, x), y) = If e x y

beginStatement = accept "begin" -# iter parse #- require "end" >-> Begin

whileStatement = accept "while" -# Expr.parse # require "do" -# parse >-> buildWhile
buildWhile (e, x) = While e x

readStatement = accept "read" -# word #- require ";" >-> Read

writeStatement = accept "write" -# Expr.parse #- require ";" >-> Write

--comment = accept "--" -# chars.length.head.lines >-> Comment
indent :: Int -> String
indent ind 
    |ind <= 0  = ""
    |otherwise = "\t"++indent (ind-1)

shw :: Int -> T -> String
shw ind (Assignment variable expression) = indent ind ++variable++":="++(toString expression)++";\n"
shw ind (If cond thenStmts elseStmts) = indent ind ++ "if " ++ (toString cond) ++ " then\n" ++ shw (ind+1) thenStmts ++ "else\n" ++ shw (ind+1) elseStmts
shw ind (Skip) = indent ind ++ "skip;\n"
shw ind (Begin beginStmts) = indent ind ++ "begin\n"++ concat (map (shw (ind+1)) beginStmts) ++ "end\n"
shw ind (While cond whileStmts) = 
shw ind (Read variable) = indent ind ++ "read " ++ variable ++ ";\n"
shw ind (Write expression) = indent ind ++ "write " ++ (toString expression) ++ ";\n"

instance Parse Statement where
  parse = {-comment ! -}assignment ! ifStatement ! skipStatement ! beginStatement ! whileStatement ! readStatement ! writeStatement
  toString = shw 0
