
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


main :: IO()
main = do
  input <- getLine
  print $ DNA.complementSequence input
  --print $ map (map toUpper) (complementRNASequence $ map toLower $ input)
