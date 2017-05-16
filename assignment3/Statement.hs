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
    Begin Statement |
    While Expr.T Statement |
	Read String |
    Write Expr.T

    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
	
skipStatement = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip

beginStatement = accept "begin" -# iter parse #- require "end" >-> Begin

whileStatement = accept "while" -# Expr.parse #- require "do" >-> While

readStatement = accept "read" -# word #- require ";" >-> Read

writeStatement = accept "write" -# Expr.parse #- require ";" >-> Write

instance Parse Statement where
  parse = assignment ! exec ! skipStatement ! beginStatement ! whileStatement ! readStatement ! writeStatement
  toString = error "Statement.toString not implemented"
