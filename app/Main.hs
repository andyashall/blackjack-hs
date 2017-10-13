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
  game (d1, dh1, ph1)

game :: Game -> IO ()
game (d, ph, dh) = do
  if busted ph
    then putStrLn "You busted!"
      else if busted dh
        then putStrLn "Dealer busted!"
        else do
          putStrLn $ "Dealer hand: " ++ id (show (sum $ map cardValue dh))
          putStrLn $ "Your hand: " ++ id (show (sum $ map cardValue ph))
          action <- getLine
          putStrLn action

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

busted :: Deck -> Bool
busted d = (sum $ map cardValue d) > 21