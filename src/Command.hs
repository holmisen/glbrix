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
   | CmdBlock Int Int Int
   | CmdBrick Int Int
   | CmdPlate Int Int
   | CmdClone
   | CmdDelete
   | CmdMove
   | CmdGroup
   | CmdUngroup
   | CmdRotate
   deriving Show

readCommand :: [Char] -> Maybe Command
readCommand = fmap fst . listToMaybe . readP_to_S (command <* eof)

command = P.choice
  [ cmdColor
  , cmdBlock
  , cmdBrick
  , cmdPlate
  , cmdClone
  , cmdDelete
  , cmdMove
  , cmdGroup
  , cmdUngroup
  , cmdRotate
  ]

cmdColor = do
   P.char 'c'
   c <- P.get
   CmdColor <$>
      case c of
         'b' -> return Blue
         'B' -> return DarkBlue
         'e' -> return Gray
         'E' -> return DarkGray
         'g' -> return Green
         'G' -> return DarkGreen
         'k' -> return Black
         'l' -> return LightBlue
         'o' -> return Brown
         'r' -> return Red
         't' -> return Tan
         'w' -> return White
         'y' -> return Yellow
         _   -> pfail

cmdPlate = do
   P.char 'p'
   CmdPlate <$> (int <* char 'x') <*> int

cmdBrick = do
   P.char 'b'
   CmdBrick <$> (int <* char 'x') <*> int

cmdBlock = do
   P.char 'B'
   CmdBlock <$> (int <* char 'x') <*> (int <* char 'x') <*> int

cmdClone = const CmdClone <$> P.char ' '

cmdDelete = const CmdDelete <$> P.char 'x'

cmdMove = const CmdMove <$> P.char 'm'

cmdGroup = const CmdGroup <$> P.char 'g'

cmdUngroup = const CmdUngroup <$> P.char 'G'

cmdRotate = const CmdRotate <$> P.char 'r'

int :: ReadP Int
int = read <$> many1 (satisfy isDigit)
