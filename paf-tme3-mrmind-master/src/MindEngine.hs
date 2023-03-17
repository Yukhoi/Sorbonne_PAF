
module MindEngine where

import Data.Foldable

-- utilitaires de séquences
import Data.Sequence (Seq, Seq (..))
import qualified Data.Sequence as Seq

-- utilitaires d'ensembles
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Foldable as F

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

initFeedback :: Secret -> Feedback
initFeedback (Secret sec _) =
  fmap (\p -> (p, Unmarked)) sec 

markCorrectOne :: Peg -> (Peg, FeedbackMark) -> (Peg, (Peg, FeedbackMark))
markCorrectOne gpeg (speg, mk) | gpeg == speg = (PEmpty, (speg, MarkedCorrect))
                               | otherwise = (gpeg, (speg, mk))

-- fonction à définir (cf. tests)                      
markCorrect :: Guess -> Feedback -> (Guess, Feedback)
markCorrect g f = (foldl fst (markCorrect_aux g f ((Seq.length g) - 1)) [], foldl snd (markCorrect_aux g f ((Seq.length g)-1)) [])

markCorrect_aux :: Guess -> Feedback -> Int -> (Guess, Feedback)
markCorrect_aux g f i = case i of
  0 -> markCorrectOne (Seq.index g i) (Seq.index f i) 
  otherwise -> markCorrectOne g f (i-1) : (markCorrectOne (Seq.index g i) (Seq.index f i))
-- fonction à définir (cf. tests)
markPosition :: Guess -> Feedback -> Feedback
markPosition _ _ = undefined

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
