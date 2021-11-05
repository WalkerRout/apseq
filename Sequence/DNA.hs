
module Sequence.DNA
( verifySequence
, complementSequence
) where


-- Imports
import Data.Char
import qualified Sequence.Data as Data
import qualified Sequence.Dictionaries as Dict


verifySequence :: Data.Sequence -> Data.Sequence
verifySequence s = [toUpper x | x <- s, toLower x == 'a' || toLower x == 't' || toLower x == 'c' || toLower x == 'g']


complementSequence :: Data.Sequence -> [Data.Sequence]
complementSequence sequ = [sequ, [Dict.nucDNADict x | x <- sequ]]
