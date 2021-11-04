
module Sequence.RNA
( nucDict
, verifySequence
, complementSequence
) where


-- Imports
import Data.Char

import qualified Sequence.Data as Data


nucDict :: Data.Base -> Data.Base
nucDict s = case s of
  'a' -> 'u'
  'u' -> 'a'
  'c' -> 'g' 
  'g' -> 'c'
  _ -> error "Not in pattern!"


verifySequence :: Data.Sequence -> Data.Sequence
verifySequence s = [x | x <- s, toLower x == 'a' || toLower x == 'u' || toLower  x == 'c' || toLower x == 'g']


complementSequence :: Data.Sequence -> [Data.Sequence]
complementSequence sequ = [s, [nucDict x | x <- s]]
  where s = verifySequence sequ