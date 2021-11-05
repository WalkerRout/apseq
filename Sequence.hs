
-- Title: APSequence
-- Description: DNA sequence verifier with amino acid mapping
-- Course: Anatomy and Physiology 12 
-- Author: Walker Rout
-- Creation Date: Wednesday, November 3rd, 2021

module Main where


-- Imports
import qualified Sequence.DNA as DNA
import qualified Sequence.RNA as RNA
import qualified Sequence.Data as Data
import qualified Sequence.Conversions as Conv
import qualified Sequence.Dictionaries as Dict


mapCodon :: [Data.Sequence] -> [Data.Amino]
mapCodon seq = map Dict.aminoDict seq


verifyCodonStart :: [Data.Sequence] -> [Data.Sequence]
verifyCodonStart seq 
  | head seq == "UAC" = seq
  | otherwise = error "Start codon required!"


verifyCodonStop :: [Data.Sequence] -> [Data.Sequence]
verifyCodonStop seq
  | lastSeq == "AUU" || lastSeq == "AUC" || lastSeq == "ACU" = seq
  | otherwise = seq ++ ["AUU"]
  where lastSeq = last seq


verifyCodonLength :: [Data.Sequence] -> [Data.Sequence]
verifyCodonLength seq
  | length (last seq) < 3 = init seq
  | otherwise = seq


splitCodon :: Data.Sequence -> [Data.Sequence]
splitCodon [] = []
splitCodon seq
  | initSeq == "AUU" || initSeq == "AUC" || initSeq == "ACU" = [initSeq]
  | otherwise = initSeq : splitCodon tailSeq
  where
    initSeq = take 3 seq
    tailSeq = drop 3 seq


mRNA = (\(_:x:_) -> x) . RNA.complementSequence . Conv.dnaToRNA . DNA.verifySequence

codon = mapCodon . verifyCodonStart . verifyCodonStop . verifyCodonLength . splitCodon

--mRNACodon = (mapCodon . verifyCodonStart . verifyCodonStop . verifyCodonLength . splitCodon . (\(_:x:_) -> x) . RNA.complementSequence . Conv.dnaToRNA . DNA.verifySequence)


main :: IO()
main = do
  putStrLn $ "Input Sense Strand Below: "
  input <- getLine
  print $ (codon . mRNA) (input)
  --print $ map (map toUpper) (complementRNASequence $ map toLower $ input)
   
