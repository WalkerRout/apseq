
module Sequence.DNA
( verifySequence
, complementSequence
) where


-- Imports
import Data.Char
import qualified Sequence.Data as Data
import qualified Sequence.Dictionarys as Dict


verifySequence :: Data.Sequence -> Data.Sequence
verifySequence s = [x | x <- s, toLower x == 'a' || toLower x == 't' || toLower x == 'c' || toLower x == 'g']


complementSequence :: Data.Sequence -> [Data.Sequence]
complementSequence sequ = [s, [Dict.nucDNADict x | x <- s]]
  where s = verifySequence sequ