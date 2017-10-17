import System.Random.Shuffle

data Card = Numeric Int | Ace | King | Queen | Jack
  deriving(Show)

type Deck = [Card]

type Game = (Deck, Deck, Deck)

main :: IO ()
main = do
  deck <- shuffleM newDeck
  let (d, ph, dh) = addCard (deck, [], [])
  game (addCard (d, dh, ph))

game :: Game -> IO ()
game (d, dh, ph) = do
  if busted ph
    then putStrLn $ "You busted! with: " ++ id (show (sum $ map cardValue ph))
      else if busted dh
        then putStrLn $ "Dealer busted! with: " ++ id (show (sum $ map cardValue dh))
        else do
          putStrLn $ "Dealer hand: " ++ id (show (sum $ map cardValue dh))
          putStrLn $ "Your hand: " ++ id (show (sum $ map cardValue ph))
          action <- getLine
          case action of
            "hit" -> do 
              let (d1, ph1, dh1) = addCard (d, ph, dh)
              game (playDealer (d1, dh1, ph1))
            "stay" -> do
              let (d1, dh1, ph1) = justDealer (d, dh, ph)
              if busted dh1
                then putStrLn $ "Dealer busted! with: " ++ id (show (sum $ map cardValue dh1))
                else declareWinner (d1, dh1, ph1)
            _ -> game (d, dh, ph)

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

playDealer :: Game -> Game
playDealer (d, dh, ph) = if (sum $ map cardValue dh) < 17
  then addCard (d, dh, ph)
  else (d, dh, ph)

justDealer :: Game -> Game
justDealer (d, dh, ph) = if (sum $ map cardValue dh) < 17
  then justDealer (addCard (d, dh, ph))
  else (d, dh, ph)

declareWinner :: Game -> IO ()
declareWinner (d, dh, ph) = if (sum $ map cardValue dh) == (sum $ map cardValue ph)
  then putStrLn $ "Its a tie! Dealers hand: " ++ id (show (sum $ map cardValue dh)) ++ " Your hand: " ++ id (show (sum $ map cardValue ph))
  else if (sum $ map cardValue dh) > (sum $ map cardValue ph)
    then putStrLn $ "Dealer wins! Dealers hand: " ++ id (show (sum $ map cardValue dh)) ++ " Your hand: " ++ id (show (sum $ map cardValue ph))
    else putStrLn $ "You win! Dealers hand: " ++ id (show (sum $ map cardValue dh)) ++ " Your hand: " ++ id (show (sum $ map cardValue ph))