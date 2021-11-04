
-- Title: APSequence
-- Description: DNA sequence verifier with amino acid mapping
-- Course: Anatomy and Physiology 12 
-- Author: Walker Rout
-- Creation Date: Wednesday, November 3rd, 2021

module Nucleotide where


-- Imports
import qualified Sequence.DNA as DNA
import qualified Sequence.RNA as RNA
import qualified Sequence.Data as Data
import qualified Sequence.Dictionarys as Dict



splitCodon :: Data.Sequence -> [Data.Sequence]
splitCodon [] = []
splitCodon seq = initSeq : splitCodon tailSeq
  where 
    initSeq = take 3 seq
    tailSeq = drop 3 seq


main :: IO()
main = do
  input <- getLine
  print $ splitCodon $ (\(_:x:_) -> x) $ RNA.complementSequence input
  --print $ map (map toUpper) (complementRNASequence $ map toLower $ input)
   
