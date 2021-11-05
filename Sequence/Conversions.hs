
module Sequence.Conversions
( rnaToDNA
, dnaToRNA
) where


-- Imports
import qualified Sequence.Data as Data
import qualified Sequence.Dictionaries as Dict


rnaToDNA :: Data.Sequence -> Data.Sequence
rnaToDNA sequ = map Dict.nucRNAToDNADict sequ


dnaToRNA :: Data.Sequence -> Data.Sequence
dnaToRNA sequ = map Dict.nucDNAToRNADict sequ
