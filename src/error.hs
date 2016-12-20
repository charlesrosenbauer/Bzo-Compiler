module BzoError where
--import BzoParser
import BzoTypes










showBzErr :: BzoErr -> String
showBzErr (StringErr   st) = "Bzo Error: " ++ (show st)
showBzErr (LexErr      st) = "Lexer Error: " ++ (show st)
showBzErr (TypeErr     st) = "Type Error: " ++ (show st)
showBzErr (Other         ) = "Unknown Error?"
instance Show BzoErr where show = showBzErr
