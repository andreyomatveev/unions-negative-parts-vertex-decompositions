-- Andrey O. Matveev
-- Calculations for Theorems 7.5, 7.7, 7.9 and 7.11 of the monograph A.O. Matveev, Symmetric Cycles,
-- Jenny Stanford Publishing, 2023, https://www.jennystanford.com/ .

module UNPVD
  ( theorem7Dot5I
  , theorem7Dot5II
  , theorem7Dot5III
  , theorem7Dot5IV
  , theorem7Dot5V
  , theorem7Dot7I
  , theorem7Dot7II
  , theorem7Dot7III
  , theorem7Dot7IV
  , theorem7Dot7V
  , theorem7Dot9I
  , theorem7Dot9II
  , theorem7Dot9III
  , theorem7Dot9IV
  , theorem7Dot9V
  , theorem7Dot9VI
  , theorem7Dot9VII
  , theorem7Dot9VIII
  , theorem7Dot9IX
  , theorem7Dot9X
  , theorem7Dot11I
  , theorem7Dot11II
  , theorem7Dot11III
  , theorem7Dot11IV
  , theorem7Dot11V
  , theorem7Dot11VI
  , theorem7Dot11VII
  , theorem7Dot11VIII
  , theorem7Dot11IX
  , theorem7Dot11X
  ) where

import           Math.Combinatorics.Exact.Binomial (choose)
import qualified SmirnovWordsModule                as SWM

{------------------------------------------------------------------------------}

-- Calculations for Theorem 7.5 of the monograph.
--
-- Given positive odd integers lCap, l', l'', and positive integers j' and j'',
-- consider the following family of ordered two-member intersecting Sperner families (A,B) that cover
-- the ground set [t] :={1, 2, ..., t}:
-- { (A,B) <- 2^[t] X 2^[t]:  |A| = j',  |B| = j'', |A\cap B| /= 0, |A\cup B| = t,
--    q(A\cap B) = lCap,  q(A-B) = l', q(B-A) = l'' } .                                              (*)

theorem7Dot5I ::
     Integer -> (Integer, Integer) -> (Integer, Integer, Integer) -> Integer
-- Theorem 7.5(i): In the family (*) there are theorem7Dot5I t (j', j'') (lCap, l', l'') pairs (A,B)
-- of sets A and B such that {1,t} \cap A = {1}, and  {1,t} \cap B = {1,t} .
-- Call for instance (as in Example 7.6(i) of the monograph)
--      ghci> theorem7Dot5I 5 (3, 4) (3, 3, 3)
-- to get the result:
--      3
theorem7Dot5I t (j', j'') (lCap, l', l'')
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | lCap < 1 || even lCap = -3 -- "N/A: lCap should be positive and odd"
  | l' < 1 || even l' = -4 -- "N/A: lCap should be positive and odd"
  | l'' < 1 || even l'' = -5 -- "N/A: lCap should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverThreeLetterAlphabet
      (SWM.Theta3, SWM.Beta3)
      ((lCap + 1) `div` 2, (l' - 1) `div` 2, (l'' + 1) `div` 2) *
    numOfComps ((lCap + 1) `div` 2) (j' + j'' - t) *
    numOfComps ((l' - 1) `div` 2) (t - j'') *
    numOfComps ((l'' + 1) `div` 2) (t - j')

theorem7Dot5II ::
     Integer -> (Integer, Integer) -> (Integer, Integer, Integer) -> Integer
-- Theorem 7.5(ii): In the family (*) there are theorem7Dot5II t (j', j'') (lCap, l', l'') pairs (A,B)
-- of sets A and B such that {1,t} \cap A = {1}, and  {1,t} \cap B = {t} .
-- Call for instance (as in Example 7.6(ii) of the monograph)
--      ghci> theorem7Dot5II 5 (3, 4) (5, 1, 3)
-- to get the result:
--      1
theorem7Dot5II t (j', j'') (lCap, l', l'')
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | lCap < 1 || even lCap = -3 -- "N/A: lCap should be positive and odd"
  | l' < 1 || even l' = -4 -- "N/A: lCap should be positive and odd"
  | l'' < 1 || even l'' = -5 -- "N/A: lCap should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverThreeLetterAlphabet
      (SWM.Theta3, SWM.Alpha3)
      ((lCap - 1) `div` 2, (l' + 1) `div` 2, (l'' + 1) `div` 2) *
    numOfComps ((lCap - 1) `div` 2) (j' + j'' - t) *
    numOfComps ((l' + 1) `div` 2) (t - j'') *
    numOfComps ((l'' + 1) `div` 2) (t - j')

theorem7Dot5III ::
     Integer -> (Integer, Integer) -> (Integer, Integer, Integer) -> Integer
-- Theorem 7.5(iii): In the family (*) there are theorem7Dot5III t (j', j'') (lCap, l', l'') pairs (A,B)
-- of sets A and B such that {1,t} \cap A = {1,t} \cap B = {1,t} .
-- Call for instance (as in Example 7.6(iii) of the monograph)
--      ghci> theorem7Dot5III 5 (4, 4) (5, 3, 3)
-- to get the result:
--      2
theorem7Dot5III t (j', j'') (lCap, l', l'')
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | lCap < 1 || even lCap = -3 -- "N/A: lCap should be positive and odd"
  | l' < 1 || even l' = -4 -- "N/A: lCap should be positive and odd"
  | l'' < 1 || even l'' = -5 -- "N/A: lCap should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverThreeLetterAlphabet
      (SWM.Theta3, SWM.Theta3)
      ((lCap + 1) `div` 2, (l' - 1) `div` 2, (l'' - 1) `div` 2) *
    numOfComps ((lCap + 1) `div` 2) (j' + j'' - t) *
    numOfComps ((l' - 1) `div` 2) (t - j'') *
    numOfComps ((l'' - 1) `div` 2) (t - j')

theorem7Dot5IV ::
     Integer -> (Integer, Integer) -> (Integer, Integer, Integer) -> Integer
-- Theorem 7.5(iv): In the family (*) there are theorem7Dot5IV t (j', j'') (lCap, l', l'') pairs (A,B)
-- of sets A and B such that {1,t} \cap A = {1,t}, and  |{1,t}\cap B| = 0 .
-- Call for instance (as in Example 7.6(iv) of the monograph)
--      ghci> theorem7Dot5IV 5 (4, 3) (3, 3, 3)
-- to get the result:
--      2
theorem7Dot5IV t (j', j'') (lCap, l', l'')
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | lCap < 1 || even lCap = -3 -- "N/A: lCap should be positive and odd"
  | l' < 1 || even l' = -4 -- "N/A: lCap should be positive and odd"
  | l'' < 1 || even l'' = -5 -- "N/A: lCap should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverThreeLetterAlphabet
      (SWM.Alpha3, SWM.Alpha3)
      ((lCap - 1) `div` 2, (l' + 1) `div` 2, (l'' - 1) `div` 2) *
    numOfComps ((lCap - 1) `div` 2) (j' + j'' - t) *
    numOfComps ((l' + 1) `div` 2) (t - j'') *
    numOfComps ((l'' - 1) `div` 2) (t - j')

theorem7Dot5V ::
     Integer -> (Integer, Integer) -> (Integer, Integer, Integer) -> Integer
-- Theorem 7.5(v): In the family (*) there are theorem7Dot5V t (j', j'') (lCap, l', l'') pairs (A,B)
-- of sets A and B such that {1,t} \cap A = {1,t}, and  {1,t}\cap B = {t} .
-- Call for instance (as in Example 7.6(v) of the monograph)
--      ghci> theorem7Dot5V 5 (3, 3) (1, 3, 5)
-- to get the result:
--      1
theorem7Dot5V t (j', j'') (lCap, l', l'')
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | lCap < 1 || even lCap = -3 -- "N/A: lCap should be positive and odd"
  | l' < 1 || even l' = -4 -- "N/A: l' should be positive and odd"
  | l'' < 1 || even l'' = -5 -- "N/A: l'' should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverThreeLetterAlphabet
      (SWM.Alpha3, SWM.Theta3)
      ((lCap + 1) `div` 2, (l' + 1) `div` 2, (l'' - 1) `div` 2) *
    numOfComps ((lCap + 1) `div` 2) (j' + j'' - t) *
    numOfComps ((l' + 1) `div` 2) (t - j'') *
    numOfComps ((l'' - 1) `div` 2) (t - j')

{------------------------------------------------------------------------------}

-- Calculations for Theorem 7.7 of the monograph.
--
-- Given positive odd integers lCap and lTriangle, and positive integers j' and j'',
-- consider the following family of ordered two-member intersecting Sperner families (A,B) that cover
-- the ground set [t] :={1, 2, ..., t}:
-- { (A,B) <- 2^[t] X 2^[t]:  |A| = j',  |B| = j'', |A\cap B| /= 0, |A\cup B| = t,
--    q(A\cap B) = lCap,  q(A\vartriangle B) = lTraingle } .                                              (**)

theorem7Dot7I :: Integer -> (Integer, Integer) -> (Integer, Integer) -> Integer
-- Theorem 7.7(i): In the family (**) there are theorem7Dot7I t (j', j'') (lCap, lTriangle) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1}, and  {1,t}\cap B = {1,t} .
-- Call for instance (as in Example 7.8(i) of the monograph)
--      ghci> theorem7Dot7I 5 (3, 4) (3, 3)
-- to get the result:
--      2
theorem7Dot7I t (j', j'') (lCap, lTriangle)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | lCap < 1 || even lCap = -3 -- "N/A: lCap should be positive and odd"
  | lTriangle < 1 || even lTriangle = -4 -- "N/A: lTriangle should be positive and odd"
  | lCap /= lTriangle = 0
  | otherwise =
    numOfComps ((lCap + 1) `div` 2) (j' + j'' - t) *
    numOfComps ((lTriangle + 1) `div` 2) (2 * t - j' - j'')

theorem7Dot7II :: Integer -> (Integer, Integer) -> (Integer, Integer) -> Integer
-- Theorem 7.7(ii): In the family (**) there are theorem7Dot7II t (j', j'') (lCap, lTriangle) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1}, and  {1,t}\cap B = {t} .
-- Call for instance (as in Example 7.8(ii) of the monograph)
--      ghci> theorem7Dot7II 5 (3, 3) (3, 3)
-- to get the result:
--      3
theorem7Dot7II t (j', j'') (lCap, lTriangle)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | lCap < 1 || even lCap = -3 -- "N/A: lCap should be positive and odd"
  | lTriangle < 1 || even lTriangle = -4 -- "N/A: lTriangle should be positive and odd"
  | lCap /= lTriangle = 0
  | otherwise =
    numOfComps ((lCap - 1) `div` 2) (j' + j'' - t) *
    numOfComps ((lTriangle + 1) `div` 2) (2 * t - j' - j'')

theorem7Dot7III ::
     Integer -> (Integer, Integer) -> (Integer, Integer) -> Integer
-- Theorem 7.7(iii): In the family (**) there are theorem7Dot7III t (j', j'') (lCap, lTriangle) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1,t}\cap B = {1,t} .
-- Call for instance (as in Example 7.8(iii) of the monograph)
--      ghci> theorem7Dot7III 5 (4, 4) (3, 3)
-- to get the result:
--      2
theorem7Dot7III t (j', j'') (lCap, lTriangle)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | lCap < 1 || even lCap = -3 -- "N/A: lCap should be positive and odd"
  | lTriangle < 1 || even lTriangle = -4 -- "N/A: lTriangle should be positive and odd"
  | lCap /= lTriangle = 0
  | otherwise =
    numOfComps ((lCap + 1) `div` 2) (j' + j'' - t) *
    numOfComps ((lTriangle - 1) `div` 2) (2 * t - j' - j'')

theorem7Dot7IV :: Integer -> (Integer, Integer) -> (Integer, Integer) -> Integer
-- Theorem 7.7(iv): In the family (**) there are theorem7Dot7IV t (j', j'') (lCap, lTriangle) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1,t}, and |{1,t}\cap B| = 0 .
-- Call for instance (as in Example 7.8(iv) of the monograph)
--      ghci> theorem7Dot7IV 5 (4, 3) (3, 3)
-- to get the result:
--      2
theorem7Dot7IV t (j', j'') (lCap, lTriangle)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | lCap < 1 || even lCap = -3 -- "N/A: lCap should be positive and odd"
  | lTriangle < 1 || even lTriangle = -4 -- "N/A: lTriangle should be positive and odd"
  | lCap /= lTriangle = 0
  | otherwise =
    numOfComps ((lCap - 1) `div` 2) (j' + j'' - t) *
    numOfComps ((lTriangle + 1) `div` 2) (2 * t - j' - j'')

theorem7Dot7V :: Integer -> (Integer, Integer) -> (Integer, Integer) -> Integer
-- Theorem 7.7(v): In the family (**) there are theorem7Dot7V t (j', j'') (lCap, lTriangle) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1,t}, and {1,t}\cap B = {t} .
-- Call for instance (as in Example 7.8(v) of the monograph)
--      ghci> theorem7Dot7V 5 (3, 4) (3, 3)
-- to get the result:
--      2
theorem7Dot7V t (j', j'') (lCap, lTriangle)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | lCap < 1 || even lCap = -3 -- "N/A: lCap should be positive and odd"
  | lTriangle < 1 || even lTriangle = -4 -- "N/A: lTriangle should be positive and odd"
  | lCap /= lTriangle = 0
  | otherwise =
    numOfComps ((lCap + 1) `div` 2) (j' + j'' - t) *
    numOfComps ((lTriangle + 1) `div` 2) (2 * t - j' - j'')

{------------------------------------------------------------------------------}

-- Calculations for Theorem 7.9 of the monograph.
--
-- Given positive odd integers l', l'', lCap, l, and positive integers j' and j'',
-- consider the following family of ordered two-member intersecting Sperner families (A,B) that do not cover
-- the ground set [t] :={1, 2, ..., t}:
-- { (A,B) <- 2^[t] X 2^[t]:  |A| = j',  |B| = j'', |A\cap B| = j /= 0, |A\cup B| < t,
--    q(A-B) = l', q(B-A) = l'', q(A\cap B) = lCap,  q(A\cup B) = l } .                                       (***)

theorem7Dot9I ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer, Integer)
  -> Integer
-- Theorem 7.9(i): In the family (***) there are theorem7Dot9I t (j', j'', j) (l', l'', lCap, l) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1,t}\cap B = {1} .
-- Call for instance (as in Example 7.10(i) of the monograph)
--      ghci> theorem7Dot9I 5 (3, 3, 2) (3, 3, 3, 1)
-- to get the result:
--      4
theorem7Dot9I t (j', j'', j) (l', l'', lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | l' < 1 || even l' = -4 -- "N/A: lCap should be positive and odd"
  | l'' < 1 || even l'' = -5 -- "N/A: lCap should be positive and odd"
  | lCap < 1 || even lCap = -6 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -7 -- "N/A: lCap should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverFourLetterAlphabet
      (SWM.Gamma4, SWM.Theta4)
      ((l + 1) `div` 2, (l' - 1) `div` 2, (l'' - 1) `div` 2, (lCap + 1) `div` 2) *
    numOfComps ((l + 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((l' - 1) `div` 2) (j' - j) *
    numOfComps ((l'' - 1) `div` 2) (j'' - j) *
    numOfComps ((lCap + 1) `div` 2) j

theorem7Dot9II ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer, Integer)
  -> Integer
-- Theorem 7.9(ii): In the family (***) there are theorem7Dot9II t (j', j'', j) (l', l'', lCap, l) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1}, and {1,t}\cap B = {1,t} .
-- Call for instance (as in Example 7.10(ii) of the monograph)
--      ghci> theorem7Dot9II 5 (3, 3, 2) (3, 1, 3, 3)
-- to get the result:
--      4
theorem7Dot9II t (j', j'', j) (l', l'', lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | l' < 1 || even l' = -4 -- "N/A: lCap should be positive and odd"
  | l'' < 1 || even l'' = -5 -- "N/A: lCap should be positive and odd"
  | lCap < 1 || even lCap = -6 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -7 -- "N/A: lCap should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverFourLetterAlphabet
      (SWM.Gamma4, SWM.Beta4)
      ((l - 1) `div` 2, (l' - 1) `div` 2, (l'' + 1) `div` 2, (lCap + 1) `div` 2) *
    numOfComps ((l - 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((l' - 1) `div` 2) (j' - j) *
    numOfComps ((l'' + 1) `div` 2) (j'' - j) *
    numOfComps ((lCap + 1) `div` 2) j

theorem7Dot9III ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer, Integer)
  -> Integer
-- Theorem 7.9(iii): In the family (***) there are theorem7Dot9III t (j', j'', j) (l', l'', lCap, l) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1}, and |{1,t}\cap B| = 0 .
-- Call for instance (as in Example 7.10(iii) of the monograph)
--      ghci> theorem7Dot9III 5 (2, 2, 1) (1, 3, 3, 1)
-- to get the result:
--      2
theorem7Dot9III t (j', j'', j) (l', l'', lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | l' < 1 || even l' = -4 -- "N/A: lCap should be positive and odd"
  | l'' < 1 || even l'' = -5 -- "N/A: lCap should be positive and odd"
  | lCap < 1 || even lCap = -6 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -7 -- "N/A: lCap should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverFourLetterAlphabet
      (SWM.Alpha4, SWM.Theta4)
      ((l + 1) `div` 2, (l' + 1) `div` 2, (l'' - 1) `div` 2, (lCap - 1) `div` 2) *
    numOfComps ((l + 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((l' + 1) `div` 2) (j' - j) *
    numOfComps ((l'' - 1) `div` 2) (j'' - j) *
    numOfComps ((lCap - 1) `div` 2) j

theorem7Dot9IV ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer, Integer)
  -> Integer
-- Theorem 7.9(iv): In the family (***) there are theorem7Dot9IV t (j', j'', j) (l', l'', lCap, l) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1}, and {1,t}\cap B = {t} .
-- Call for instance (as in Example 7.10(iv) of the monograph)
--      ghci> theorem7Dot9IV 5 (2, 3, 1) (1, 3, 3, 3)
-- to get the result:
--      4
theorem7Dot9IV t (j', j'', j) (l', l'', lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | l' < 1 || even l' = -4 -- "N/A: lCap should be positive and odd"
  | l'' < 1 || even l'' = -5 -- "N/A: lCap should be positive and odd"
  | lCap < 1 || even lCap = -6 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -7 -- "N/A: lCap should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverFourLetterAlphabet
      (SWM.Alpha4, SWM.Beta4)
      ((l - 1) `div` 2, (l' + 1) `div` 2, (l'' + 1) `div` 2, (lCap - 1) `div` 2) *
    numOfComps ((l - 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((l' + 1) `div` 2) (j' - j) *
    numOfComps ((l'' + 1) `div` 2) (j'' - j) *
    numOfComps ((lCap - 1) `div` 2) j

theorem7Dot9V ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer, Integer)
  -> Integer
-- Theorem 7.9(v): In the family (***) there are theorem7Dot9V t (j', j'', j) (l', l'', lCap, l) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1,t}\cap B = {1,t} .
-- Call for instance (as in Example 7.10(v) of the monograph)
--      ghci> theorem7Dot9V 5 (3, 3, 2) (3, 3, 3, 3)
-- to get the result:
--      6
theorem7Dot9V t (j', j'', j) (l', l'', lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | l' < 1 || even l' = -4 -- "N/A: lCap should be positive and odd"
  | l'' < 1 || even l'' = -5 -- "N/A: lCap should be positive and odd"
  | lCap < 1 || even lCap = -6 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -7 -- "N/A: lCap should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverFourLetterAlphabet
      (SWM.Gamma4, SWM.Gamma4)
      ((l - 1) `div` 2, (l' - 1) `div` 2, (l'' - 1) `div` 2, (lCap + 1) `div` 2) *
    numOfComps ((l - 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((l' - 1) `div` 2) (j' - j) *
    numOfComps ((l'' - 1) `div` 2) (j'' - j) *
    numOfComps ((lCap + 1) `div` 2) j

theorem7Dot9VI ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer, Integer)
  -> Integer
-- Theorem 7.9(vi): In the family (***) there are theorem7Dot9VI t (j', j'', j) (l', l'', lCap, l) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1,t}, and |{1,t}\cap B| = 0 .
-- Call for instance (as in Example 7.10(vi) of the monograph)
--      ghci> theorem7Dot9VI 5 (3, 2, 1) (3, 3, 3, 3)
-- to get the result:
--      6
theorem7Dot9VI t (j', j'', j) (l', l'', lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | l' < 1 || even l' = -4 -- "N/A: lCap should be positive and odd"
  | l'' < 1 || even l'' = -5 -- "N/A: lCap should be positive and odd"
  | lCap < 1 || even lCap = -6 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -7 -- "N/A: lCap should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverFourLetterAlphabet
      (SWM.Alpha4, SWM.Alpha4)
      ((l - 1) `div` 2, (l' + 1) `div` 2, (l'' - 1) `div` 2, (lCap - 1) `div` 2) *
    numOfComps ((l - 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((l' + 1) `div` 2) (j' - j) *
    numOfComps ((l'' - 1) `div` 2) (j'' - j) *
    numOfComps ((lCap - 1) `div` 2) j

theorem7Dot9VII ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer, Integer)
  -> Integer
-- Theorem 7.9(vii): In the family (***) there are theorem7Dot9VII t (j', j'', j) (l', l'', lCap, l) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1,t}, and {1,t}\cap B = {t} .
-- Call for instance (as in Example 7.10(vii) of the monograph)
--      ghci> theorem7Dot9VII 5 (2, 2, 1) (1, 3, 1, 5)
-- to get the result:
--      1
theorem7Dot9VII t (j', j'', j) (l', l'', lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | l' < 1 || even l' = -4 -- "N/A: lCap should be positive and odd"
  | l'' < 1 || even l'' = -5 -- "N/A: lCap should be positive and odd"
  | lCap < 1 || even lCap = -6 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -7 -- "N/A: lCap should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverFourLetterAlphabet
      (SWM.Alpha4, SWM.Gamma4)
      ((l - 1) `div` 2, (l' + 1) `div` 2, (l'' - 1) `div` 2, (lCap + 1) `div` 2) *
    numOfComps ((l - 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((l' + 1) `div` 2) (j' - j) *
    numOfComps ((l'' - 1) `div` 2) (j'' - j) *
    numOfComps ((lCap + 1) `div` 2) j

theorem7Dot9VIII ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer, Integer)
  -> Integer
-- Theorem 7.9(viii): In the family (***) there are theorem7Dot9VIII t (j', j'', j) (l', l'', lCap, l) pairs (A,B)
-- of sets A and B such that |{1,t}\cap A| = |{1,t}\cap B| = 0 .
-- Call for instance (as in Example 7.10(viii) of the monograph)
--      ghci> theorem7Dot9VIII 5 (2, 2, 1) (3, 3, 3, 3)
-- to get the result:
--      6
theorem7Dot9VIII t (j', j'', j) (l', l'', lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | l' < 1 || even l' = -4 -- "N/A: lCap should be positive and odd"
  | l'' < 1 || even l'' = -5 -- "N/A: lCap should be positive and odd"
  | lCap < 1 || even lCap = -6 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -7 -- "N/A: lCap should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverFourLetterAlphabet
      (SWM.Theta4, SWM.Theta4)
      ((l + 1) `div` 2, (l' - 1) `div` 2, (l'' - 1) `div` 2, (lCap - 1) `div` 2) *
    numOfComps ((l + 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((l' - 1) `div` 2) (j' - j) *
    numOfComps ((l'' - 1) `div` 2) (j'' - j) *
    numOfComps ((lCap - 1) `div` 2) j

theorem7Dot9IX ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer, Integer)
  -> Integer
-- Theorem 7.9(ix): In the family (***) there are theorem7Dot9IX t (j', j'', j) (l', l'', lCap, l) pairs (A,B)
-- of sets A and B such that |{1,t}\cap A| = 0, and {1,t}\cap B = {t} .
-- Call for instance (as in Example 7.10(ix) of the monograph)
--      ghci> theorem7Dot9IX 5 (3, 2, 1) (3, 1, 3, 1)
-- to get the result:
--      2
theorem7Dot9IX t (j', j'', j) (l', l'', lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | l' < 1 || even l' = -4 -- "N/A: lCap should be positive and odd"
  | l'' < 1 || even l'' = -5 -- "N/A: lCap should be positive and odd"
  | lCap < 1 || even lCap = -6 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -7 -- "N/A: lCap should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverFourLetterAlphabet
      (SWM.Theta4, SWM.Beta4)
      ((l + 1) `div` 2, (l' - 1) `div` 2, (l'' + 1) `div` 2, (lCap - 1) `div` 2) *
    numOfComps ((l + 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((l' - 1) `div` 2) (j' - j) *
    numOfComps ((l'' + 1) `div` 2) (j'' - j) *
    numOfComps ((lCap - 1) `div` 2) j

theorem7Dot9X ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer, Integer)
  -> Integer
-- Theorem 7.9(x): In the family (***) there are theorem7Dot9X t (j', j'', j) (l', l'', lCap, l) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1,t}\cap B = {t} .
-- Call for instance (as in Example 7.10(x) of the monograph)
--      ghci> theorem7Dot9X 5 (2, 2, 1) (3, 3, 1, 3)
-- to get the result:
--      4
theorem7Dot9X t (j', j'', j) (l', l'', lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | l' < 1 || even l' = -4 -- "N/A: lCap should be positive and odd"
  | l'' < 1 || even l'' = -5 -- "N/A: lCap should be positive and odd"
  | lCap < 1 || even lCap = -6 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -7 -- "N/A: lCap should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverFourLetterAlphabet
      (SWM.Theta4, SWM.Gamma4)
      ((l + 1) `div` 2, (l' - 1) `div` 2, (l'' - 1) `div` 2, (lCap + 1) `div` 2) *
    numOfComps ((l + 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((l' - 1) `div` 2) (j' - j) *
    numOfComps ((l'' - 1) `div` 2) (j'' - j) *
    numOfComps ((lCap + 1) `div` 2) j

{------------------------------------------------------------------------------}

-- Calculations for Theorem 7.11 of the monograph.
--
-- Given positive odd integers lTriangle, lCap, l, and positive integers j' and j'',
-- consider the following family of ordered two-member intersecting Sperner families (A,B) that do not cover
-- the ground set [t] :={1, 2, ..., t}:
-- { (A,B) <- 2^[t] X 2^[t]:  |A| = j',  |B| = j'', |A\cap B| = j /= 0, |A\cup B| < t,
--    q(A\vartriangle B) = lTriangle, q(A\cap B) = lCap, q(A\cup B) = l } .                                       (****)

theorem7Dot11I ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer)
  -> Integer
-- Theorem 7.11(i): In the family (****) there are theorem7Dot11I t (j', j'', j) (lTriangle, lCap, l) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1,t}\cap B = {1} .
-- Call for instance (as in Example 7.12(i) of the monograph)
--      ghci> theorem7Dot11I 5 (2, 2, 1) (3, 1, 3)
-- to get the result:
--      1
theorem7Dot11I t (j', j'', j) (lTriangle, lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | lTriangle < 1 || even lTriangle = -4 -- "N/A: lTriangle should be positive and odd"
  | lCap < 1 || even lCap = -5 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -6 -- "N/A: l should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverThreeLetterAlphabet
      (SWM.Beta3, SWM.Theta3)
      ((l + 1) `div` 2, (lTriangle - 1) `div` 2, (lCap + 1) `div` 2) *
    numOfComps ((l + 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((lTriangle - 1) `div` 2) (j' + j'' - 2 * j) *
    numOfComps ((lCap + 1) `div` 2) j

theorem7Dot11II ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer)
  -> Integer
-- Theorem 7.11(ii): In the family (****) there are theorem7Dot11II t (j', j'', j) (lTriangle, lCap, l) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1}, and {1,t}\cap B = {1,t} .
-- Call for instance (as in Example 7.12(ii) of the monograph)
--      ghci> theorem7Dot11II 5 (3, 3, 2) (1, 3, 3)
-- to get the result:
--      1
theorem7Dot11II t (j', j'', j) (lTriangle, lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | lTriangle < 1 || even lTriangle = -4 -- "N/A: lTriangle should be positive and odd"
  | lCap < 1 || even lCap = -5 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -6 -- "N/A: l should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverThreeLetterAlphabet
      (SWM.Beta3, SWM.Alpha3)
      ((l - 1) `div` 2, (lTriangle + 1) `div` 2, (lCap + 1) `div` 2) *
    numOfComps ((l - 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((lTriangle + 1) `div` 2) (j' + j'' - 2 * j) *
    numOfComps ((lCap + 1) `div` 2) j

theorem7Dot11III ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer)
  -> Integer
-- Theorem 7.11(iii): In the family (****) there are theorem7Dot11III t (j', j'', j) (lTriangle, lCap, l) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1}, and |{1,t}\cap B| = 0 .
-- Call for instance (as in Example 7.12(iii) of the monograph)
--      ghci> theorem7Dot11III 5 (3, 2, 1) (1, 3, 1)
-- to get the result:
--      1
theorem7Dot11III t (j', j'', j) (lTriangle, lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | lTriangle < 1 || even lTriangle = -4 -- "N/A: lTriangle should be positive and odd"
  | lCap < 1 || even lCap = -5 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -6 -- "N/A: l should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverThreeLetterAlphabet
      (SWM.Alpha3, SWM.Theta3)
      ((l + 1) `div` 2, (lTriangle + 1) `div` 2, (lCap - 1) `div` 2) *
    numOfComps ((l + 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((lTriangle + 1) `div` 2) (j' + j'' - 2 * j) *
    numOfComps ((lCap - 1) `div` 2) j

theorem7Dot11IV ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer)
  -> Integer
-- Theorem 7.11(iv): In the family (****) there are theorem7Dot11IV t (j', j'', j) (lTriangle, lCap, l) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1}, and {1,t}\cap B = {t} .
-- Call for instance (as in Example 7.12(iv) of the monograph)
--      ghci> theorem7Dot11IV 5 (3, 3, 2) (3, 5, 3)
-- to get the result:
--      1
theorem7Dot11IV t (j', j'', j) (lTriangle, lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | lTriangle < 1 || even lTriangle = -4 -- "N/A: lTriangle should be positive and odd"
  | lCap < 1 || even lCap = -5 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -6 -- "N/A: l should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverThreeLetterAlphabet
      (SWM.Alpha3, SWM.Alpha3)
      ((l - 1) `div` 2, (lTriangle + 1) `div` 2, (lCap - 1) `div` 2) *
    numOfComps ((l - 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((lTriangle + 1) `div` 2) (j' + j'' - 2 * j) *
    numOfComps ((lCap - 1) `div` 2) j

theorem7Dot11V ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer)
  -> Integer
-- Theorem 7.11(v): In the family (****) there are theorem7Dot11V t (j', j'', j) (lTriangle, lCap, l) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1,t}\cap B = {1,t} .
-- Call for instance (as in Example 7.12(v) of the monograph)
--      ghci> theorem7Dot11V 5 (3, 3, 2) (3, 3, 3)
-- to get the result:
--      2
theorem7Dot11V t (j', j'', j) (lTriangle, lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | lTriangle < 1 || even lTriangle = -4 -- "N/A: lTriangle should be positive and odd"
  | lCap < 1 || even lCap = -5 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -6 -- "N/A: l should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverThreeLetterAlphabet
      (SWM.Beta3, SWM.Beta3)
      ((l - 1) `div` 2, (lTriangle - 1) `div` 2, (lCap + 1) `div` 2) *
    numOfComps ((l - 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((lTriangle + 1) `div` 2) (j' + j'' - 2 * j) *
    numOfComps ((lCap - 1) `div` 2) j

theorem7Dot11VI ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer)
  -> Integer
-- Theorem 7.11(vi): In the family (****) there are theorem7Dot11VI t (j', j'', j) (lTriangle, lCap, l) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1,t}, and |{1,t}\cap B| = 0 .
-- Call for instance (as in Example 7.12(vi) of the monograph)
--      ghci> theorem7Dot11VI 5 (3, 2, 1) (3, 3, 3)
-- to get the result:
--      4
theorem7Dot11VI t (j', j'', j) (lTriangle, lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | lTriangle < 1 || even lTriangle = -4 -- "N/A: lTriangle should be positive and odd"
  | lCap < 1 || even lCap = -5 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -6 -- "N/A: l should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverThreeLetterAlphabet
      (SWM.Alpha3, SWM.Alpha3)
      ((l - 1) `div` 2, (lTriangle + 1) `div` 2, (lCap - 1) `div` 2) *
    numOfComps ((l - 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((lTriangle + 1) `div` 2) (j' + j'' - 2 * j) *
    numOfComps ((lCap - 1) `div` 2) j

theorem7Dot11VII ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer)
  -> Integer
-- Theorem 7.11(vii): In the family (****) there are theorem7Dot11VII t (j', j'', j) (lTriangle, lCap, l) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1,t}, and {1,t}\cap B = {t} .
-- Call for instance (as in Example 7.12(vii) of the monograph)
--      ghci> theorem7Dot11VII 5 (2, 2, 1) (3, 1, 3)
-- to get the result:
--      4
theorem7Dot11VII t (j', j'', j) (lTriangle, lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | lTriangle < 1 || even lTriangle = -4 -- "N/A: lTriangle should be positive and odd"
  | lCap < 1 || even lCap = -5 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -6 -- "N/A: l should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverThreeLetterAlphabet
      (SWM.Alpha3, SWM.Beta3)
      ((l - 1) `div` 2, (lTriangle + 1) `div` 2, (lCap + 1) `div` 2) *
    numOfComps ((l - 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((lTriangle + 1) `div` 2) (j' + j'' - 2 * j) *
    numOfComps ((lCap + 1) `div` 2) j

theorem7Dot11VIII ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer)
  -> Integer
-- Theorem 7.11(viii): In the family (****) there are theorem7Dot11VIII t (j', j'', j) (lTriangle, lCap, l) pairs (A,B)
-- of sets A and B such that |{1,t}\cap A| = |{1,t}\cap B| = 0 .
-- Call for instance (as in Example 7.12(viii) of the monograph)
--      ghci> theorem7Dot11VIII 5 (2, 2, 1) (3, 3, 3)
-- to get the result:
--      2
theorem7Dot11VIII t (j', j'', j) (lTriangle, lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | lTriangle < 1 || even lTriangle = -4 -- "N/A: lTriangle should be positive and odd"
  | lCap < 1 || even lCap = -5 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -6 -- "N/A: l should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverThreeLetterAlphabet
      (SWM.Theta3, SWM.Theta3)
      ((l + 1) `div` 2, (lTriangle - 1) `div` 2, (lCap - 1) `div` 2) *
    numOfComps ((l + 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((lTriangle - 1) `div` 2) (j' + j'' - 2 * j) *
    numOfComps ((lCap - 1) `div` 2) j

theorem7Dot11IX ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer)
  -> Integer
-- Theorem 7.11(ix): In the family (****) there are theorem7Dot11IX t (j', j'', j) (lTriangle, lCap, l) pairs (A,B)
-- of sets A and B such that |{1,t}\cap A| = 0, and {1,t}\cap B = {t} .
-- Call for instance (as in Example 7.12(ix) of the monograph)
--      ghci> theorem7Dot11IX 5 (2, 2, 1) (3, 3, 3)
-- to get the result:
--      3
theorem7Dot11IX t (j', j'', j) (lTriangle, lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | lTriangle < 1 || even lTriangle = -4 -- "N/A: lTriangle should be positive and odd"
  | lCap < 1 || even lCap = -5 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -6 -- "N/A: l should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverThreeLetterAlphabet
      (SWM.Theta3, SWM.Alpha3)
      ((l + 1) `div` 2, (lTriangle + 1) `div` 2, (lCap - 1) `div` 2) *
    numOfComps ((l + 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((lTriangle + 1) `div` 2) (j' + j'' - 2 * j) *
    numOfComps ((lCap - 1) `div` 2) j

theorem7Dot11X ::
     Integer
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer)
  -> Integer
-- Theorem 7.11(x): In the family (****) there are theorem7Dot11X t (j', j'', j) (lTriangle, lCap, l) pairs (A,B)
-- of sets A and B such that {1,t}\cap A = {1,t}\cap B = {t} .
-- Call for instance (as in Example 7.12(x) of the monograph)
--      ghci> theorem7Dot11X 5 (2, 2, 1) (3, 1, 3)
-- to get the result:
--      1
theorem7Dot11X t (j', j'', j) (lTriangle, lCap, l)
  | j' <= 0 || j' >= t = -1 -- "N/A: j' should be between 1 (included) and (t-1) (included)"
  | j'' <= 0 || j'' >= t = -2 -- "N/A: j'' should be between 1 (included) and (t-1) (included)"
  | j <= 0 || j > min j' j'' - 1 = -3 -- "N/A: j should be between 1 (included) and (min {j', j''} - 1) (included)"
  | lTriangle < 1 || even lTriangle = -4 -- "N/A: lTriangle should be positive and odd"
  | lCap < 1 || even lCap = -5 -- "N/A: lCap should be positive and odd"
  | l < 1 || even l = -6 -- "N/A: l should be positive and odd"
  | otherwise =
    SWM.numberOfSmirnovWordsOverThreeLetterAlphabet
      (SWM.Theta3, SWM.Beta3)
      ((l + 1) `div` 2, (lTriangle - 1) `div` 2, (lCap + 1) `div` 2) *
    numOfComps ((l + 1) `div` 2) (t - j' - j'' + j) *
    numOfComps ((lTriangle - 1) `div` 2) (j' + j'' - 2 * j) *
    numOfComps ((lCap + 1) `div` 2) j

numOfComps :: Integer -> Integer -> Integer
numOfComps numOfPositiveParts positiveInteger =
  choose (positiveInteger - 1) (numOfPositiveParts - 1)
