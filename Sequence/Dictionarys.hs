
module Sequence.Dictionarys
( aminoDict
, nucRNADict
, nucDNADict
) where


-- Imports
import qualified Sequence.Data as Data


aminoDict :: Data.Sequence -> Data.Sequence
aminoDict s
  | s == "UUA" || s == "UUG" || s == "CUU" || s == "CUC" || s == "CUA" || s == "CUG" = "Leu"
  | s == "UCU" || s == "UCC" || s == "UCA" || s == "UCG" || s == "AGU" || s == "AGC" = "Ser"
  | s == "CGU" || s == "CGC" || s == "CGA" || s == "CGG" || s == "AGA" || s == "AGG" = "Arg"
  | s == "GGU" || s == "GGC" || s == "GGA" || s == "GGG" = "Gly"
  | s == "GUU" || s == "GUC" || s == "GUA" || s == "GUG" = "Val"
  | s == "CCU" || s == "CCC" || s == "CCA" || s == "CCG" = "Pro"
  | s == "ACU" || s == "ACC" || s == "ACA" || s == "ACG" = "Thr"
  | s == "GCU" || s == "GCC" || s == "GCA" || s == "GCG" = "Ala"
  | s == "UAA" || s == "UAG" || s == "UGA" = "STOP"
  | s == "AUU" || s == "AUC" || s == "AUA" = "Ile"
  | s == "UAU" || s == "UAC" = "Tyr"
  | s == "UUU" || s == "UUC" = "Phe"
  | s == "CAU" || s == "CAC" = "His"
  | s == "CAA" || s == "CAG" = "Gln"
  | s == "AAU" || s == "AAC" = "Asn"
  | s == "AAA" || s == "AAG" = "Lys"
  | s == "GAU" || s == "GAC" = "Asp"
  | s == "GAA" || s == "GAG" = "Glu"
  | s == "UGU" || s == "UGC" = "Cys"
  | s == "AUG" = "Met"
  | s == "UGG" = "Trp"
  | otherwise = error "Not valid codon!"


nucDNADict :: Data.Base -> Data.Base
nucDNADict s = case s of
  'A' -> 'T'
  'T' -> 'A'
  'C' -> 'G' 
  'G' -> 'C'
  _ -> error "Not in pattern!"


nucRNADict :: Data.Base -> Data.Base
nucRNADict s = case s of
  'A' -> 'U'
  'U' -> 'A'
  'C' -> 'G' 
  'G' -> 'C'
  _ -> error "Not in pattern!"
