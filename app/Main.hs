import System.Random.Shuffle

data Card = Numeric Int | Ace | King | Queen | Jack
  deriving(Show)

type Deck = [Card]

type Game = (Deck, Deck, Deck)

main :: IO ()
main = do
  deck <- shuffleM newDeck
  let (d, dh, ph) = addCard (deck, [], [])
  let (d1, dh1, ph1) = addCard (d, ph, dh)
  print $ map cardValue dh1

newDeck :: Deck
newDeck = concat $ replicate 4 $ map Numeric [2..10] ++ [Ace, King, Queen, Jack]

addCard :: Game -> Game
addCard ((d:eck), h, o) = (eck, h ++ [d], o)

cardValue :: Card -> Int
cardValue Ace = 11
cardValue King = 10
cardValue Queen = 10
cardValue Jack = 10
cardValue (Numeric i) = i 