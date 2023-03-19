
module MindEngine where

import Data.Foldable

-- utilitaires de séquences
import Data.Sequence (Seq, Seq (..))
import qualified Data.Sequence as Seq

-- utilitaires d'ensembles
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Foldable as F
import qualified Data.Maybe as Maybe


import Debug.Trace

-- Note: 8 colors because it's the standard ANSI colors
data Peg =
  PEmpty
  | Black
  | Blue
  | Green
  | Yellow
  | Cyan
  | White
  | Magenta
  | Red
  deriving (Show, Eq, Ord)

data FeedbackMark =
  MarkedCorrect
  | MarkedPosition
  | Unmarked
  deriving (Show, Eq)

data Secret = Secret { pegs :: (Seq Peg)
                     , size :: Int }
            deriving (Show, Eq)


-- smart constructor for secrets
mkSecret :: Seq Peg -> Secret
mkSecret pegs = Secret pegs (length pegs)
                
type Guess = Seq Peg
type Feedback = Seq (Peg, FeedbackMark)

data Answer = Answer { correct :: Int, position :: Int }
  deriving (Show, Eq)

-- runtime error if not a good guess
safeGuess :: Secret -> Guess -> Guess
safeGuess secret guess =
  if (size secret) /= (length guess)
  then error "Wrong guess size (please report)"
  else guess

wrongGuess :: Secret -> Guess -> Bool
wrongGuess secret guess = (size secret) /= length guess

--make Unmark for all
initFeedback :: Secret -> Feedback
initFeedback (Secret sec _) =
  fmap (\p -> (p, Unmarked)) sec 

--
markCorrectOne :: Peg -> (Peg, FeedbackMark) -> (Peg, (Peg, FeedbackMark))
markCorrectOne gpeg (speg, mk) | gpeg == speg = (PEmpty, (speg, MarkedCorrect))
                               | otherwise = (gpeg, (speg, mk))

-- fonction à définir (cf. tests)                      
markCorrect :: Guess -> Feedback -> (Guess,Feedback)
markCorrect g f = (Seq.fromList (markCorrect_aux_l g f (Seq.length f -1)),
                  Seq.fromList (markCorrect_aux_r g f (Seq.length f -1)))

markCorrect_aux_l :: Guess -> Feedback -> Int -> [Peg]
markCorrect_aux_l g f i = case i of
  0 -> [fst $ markCorrectOne (Seq.index g i) (Seq.index f i)]
  otherwise -> markCorrect_aux_l g f (i-1) ++ [fst (markCorrectOne (Seq.index g i) (Seq.index f i))]

markCorrect_aux_r :: Guess -> Feedback -> Int -> [(Peg, FeedbackMark)]
markCorrect_aux_r g f i = case i of
  0 -> [snd $ markCorrectOne (Seq.index g i) (Seq.index f i)]
  otherwise -> markCorrect_aux_r g f (i-1) ++ [snd (markCorrectOne (Seq.index g i) (Seq.index f i))]

-- fonction à définir (cf. tests)
markPosition :: Guess -> Feedback -> Feedback
markPosition g f = Seq.mapWithIndex (\id (p,fbm) -> case (p, fbm) of 
    (_, MarkedCorrect) -> (p, fbm)
    otherwise -> (if Maybe.isJust (Seq.elemIndexL p g)
                  then (if Just id == Seq.elemIndexL (p,fbm) f
                        then (p, MarkedPosition)
                        else (if Just (compare (List.elemIndex id (Seq.elemIndicesL (p,fbm) f))  (Just $ List.length (Seq.elemIndicesL p g))) == Just LT
                              then (p, MarkedPosition)
                              else (p, fbm)))
                  else (p, fbm))
    ) f


verify :: Secret -> Guess -> Answer
verify secret guess = 
  let (guess', fb) = markCorrect (safeGuess secret guess) (initFeedback secret)
      fb' = markPosition guess' fb
  in foldr verifyAux (Answer 0 0) (fmap snd fb')
  where verifyAux :: FeedbackMark -> Answer -> Answer
        verifyAux MarkedCorrect (Answer cor pos)  = Answer (cor + 1) pos 
        verifyAux MarkedPosition (Answer cor pos)  = Answer cor (pos + 1)
        verifyAux _ ans = ans

winning :: Secret -> Answer -> Bool
winning (Secret _ size) (Answer cor _) = size == cor 
