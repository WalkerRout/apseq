
-- Title: APSequence
-- Description: DNA sequence verifier with amino acid mapping
-- Course: Anatomy and Physiology 12 
-- Author: Walker Rout
-- Creation Date: Wednesday, November 3rd, 2021

module Nucleotide where


-- Imports (N/A)
import qualified Sequence.DNA as DNA
import qualified Sequence.RNA as RNA
import qualified Sequence.Data as Data


aminoDict :: Data.Sequence -> Data.Sequence
aminoDict s
  | s == "ATC" || s == "ATC" = "A"
  | otherwise = "A"

main :: IO()
main = do
  input <- getLine
  print $ DNA.verifySequence "ABC"
  --print $ map (map toUpper) (complementRNASequence $ map toLower $ input)
