module Command
  ( Command(..)
  , readCommand
  )
where

import Data.Char (isDigit)
import Data.Maybe
import Text.ParserCombinators.ReadP as P

import Types (Color(..))


data Command
   = CmdColor Color
   | CmdPlate Int Int
   | CmdBrick Int Int
   deriving Show

readCommand :: [Char] -> Maybe Command
readCommand = fmap fst . listToMaybe . readP_to_S (command <* eof)

command = P.choice [ cmdColor, cmdPlate, cmdBrick ]

cmdColor = do
   P.char 'c'
   c <- P.get
   CmdColor <$>
      case c of
         'b' -> return Blue
         'g' -> return Green
         'l' -> return Black
         'r' -> return Red
         'w' -> return White
         'y' -> return Yellow
         _   -> pfail

cmdPlate = do
   P.char 'p'
   CmdPlate <$> (int <* char 'x') <*> int

cmdBrick = do
   P.char 'b'
   CmdBrick <$> (int <* char 'x') <*> int

int :: ReadP Int
int = read <$> many1 (satisfy isDigit)
