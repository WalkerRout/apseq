
module Sequence.Dictionaries
( aminoDict
, nucRNADict
, nucDNADict
) where


-- Imports
import qualified Sequence.Data as Data


-- tRNA Anti-Codons
aminoDict :: Data.Sequence -> Data.Amino
aminoDict s
  | s == "AAU" || s == "AAC" || s == "GAA" || s == "GAG" || s == "GAU" || s == "GAC" = "Leu"
  | s == "AGA" || s == "AGG" || s == "AGU" || s == "AGC" || s == "UCA" || s == "UCG" = "Ser"
  | s == "GCA" || s == "GCG" || s == "GCU" || s == "GCC" || s == "UCU" || s == "UCC" = "Arg"
  | s == "CCA" || s == "CCG" || s == "CCU" || s == "CCC" = "Gly"
  | s == "CAA" || s == "CAG" || s == "CAU" || s == "CAC" = "Val"
  | s == "GGA" || s == "GGG" || s == "GGU" || s == "GGC" = "Pro"
  | s == "UGA" || s == "UGG" || s == "UGU" || s == "UGC" = "Thr"
  | s == "CGA" || s == "CGG" || s == "CGU" || s == "CGC" = "Ala"
  | s == "AUU" || s == "AUC" || s == "ACU" = "STOP"
  | s == "UAA" || s == "UAG" || s == "UAU" = "Ile"
  | s == "AUA" || s == "AUG" = "Tyr"
  | s == "AAA" || s == "AAG" = "Phe"
  | s == "GUA" || s == "GUG" = "His"
  | s == "GUU" || s == "GUC" = "Gln"
  | s == "UUA" || s == "UUG" = "Asn"
  | s == "UUU" || s == "UUC" = "Lys"
  | s == "CUA" || s == "CUG" = "Asp"
  | s == "CUU" || s == "CUC" = "Glu"
  | s == "ACA" || s == "ACG" = "Cys"
  | s == "UAC" = "Met"
  | s == "ACC" = "Trp"
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

