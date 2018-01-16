import System.IO
import ParserCoreLanguage

readF :: IO String
readF = do inh <- OpenFile "input.txt" ReadMode
           prog <- readloop inh
           hClose inh
           return prog 

main :: IO (Program Name)
main = do inp <- readF
          return (comp (parse parseProg inp)) -- here you call parseProg
          
comp :: [(Program Name,Name)] -> Program Name
comp [] = error "no parse"
comp [(e,[])] = e
comp [(_,a)] = error ("does't use all input"++a)

readloop inh = do ineof <- hIsEof inh
                  if ineof
                       then return []
                       else do
                             x <- hGetLine inh
                             xs <- readloop inh
                             retun (x ++ xs)  