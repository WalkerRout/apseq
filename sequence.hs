-- Title: APSequence
-- Description: DNA sequence verfier with amino acid mapping
-- Course: Anatomy and Physiology 12 
-- Author: Walker Rout
-- Creation Date: Wednesday, Novemeber 3rd, 2021

module Nucleotide
( nucRNADict
, nucDNADict
, verifyRNASequence
, verifyDNASequence
, complementRNASequence
, complementDNASequence
) where


-- Imports
import Data.Char


-- Data Initialization (N/A)


-- Type Initialization
type Sequence = String


nucDNADict s = case s of
  'a' -> 't'
  't' -> 'a'
  'c' -> 'g' 
  'g' -> 'c'


nucRNADict s = case s of
  'a' -> 'u'
  'u' -> 'a'
  'c' -> 'g' 
  'g' -> 'c'


aminoDict s
  | s == "ATC" || s == "ATC" = "A"

  
verifyRNASequence :: Sequence -> [Char]
verifyRNASequence s = [x | x <- s, toLower x == 'a' || toLower x == 'u' || toLower  x == 'c' || toLower x == 'g']


verifyDNASequence :: Sequence -> [Char]
verifyDNASequence s = [x | x <- s, toLower x == 'a' || toLower x == 't' || toLower x == 'c' || toLower x == 'g']


complementRNASequence :: Sequence -> [Sequence]
complementRNASequence seq = [s, [nucRNADict x | x <- s]]
  where s = verifyRNASequence seq


complementDNASequence :: Sequence -> [Sequence]
complementDNASequence seq = [s, [nucDNADict x | x <- s]]
  where s = verifyDNASequence seq


main :: IO()
main = do
  input <- getLine
  print $ "Hello" 
  --print $ map (map toUpper) (complementRNASequence $ map toLower $ input)
