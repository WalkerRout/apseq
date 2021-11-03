-- Title: APSequence
-- Description: DNA sequence verfier with amino acid mapping 
-- Author: Walker Rout
-- Creation Date: Wednesday, Novemeber 3rd, 2021


module Nucleotide
( nucDict
, verifySequence
, complementSequence
) where


-- Data Initialization (N/A)


-- Type Initialization
type Sequence = String


nucDict s = case s of
  'A' -> 'T'
  'T' -> 'A'
  'C' -> 'G' 
  'G' -> 'C'


aminoDict s = case s of
  

verifySequence :: Sequence -> [Char]
verifySequence s = [x | x <- s, x == 'A' || x == 'T' || x == 'C' || x == 'G']


complementSequence :: Sequence -> [Sequence]
complementSequence seq = [s, [nucDict x | x <- s]]
  where s = verifySequence seq


main :: IO()
main = do
  input <- getLine 
  print $ complementSequence input

