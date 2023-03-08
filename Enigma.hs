{-- Assignment 2: Enigma
This aim of this project was to recreate the enigma machine with steckering and attempt to break the enigma machine using bombes.
--}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type

{- Part 1: Simulation of the Enigma -}

  type Rotor = (String, Int)
  type Reflector = [(Char, Char)]
  type Offsets = (Int, Int, Int)
  type Stecker = [(Char, Char)]
  
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  --encodeMessage simply takes a string and enigma setting and cleans and returns an encoded message according to those settings.
  encodeMessage :: String -> Enigma -> String
  encodeMessage [] _ = ""
  encodeMessage string (SimpleEnigma lr mr rr reflector (l,m,r)) = encodeClean (cleanInput(string)) (SimpleEnigma lr mr rr reflector (l,m,r))
  encodeMessage string (SteckeredEnigma lr mr rr reflector (l,m,r) stecker) = encodeClean (cleanInput(string)) (SteckeredEnigma lr mr rr reflector (l,m,r) stecker)

  --encodeClean takes the cleaned string input from the use of cleanInput in encodeMessage and returns the corresponding encoded message.
  encodeClean :: String -> Enigma -> String
  encodeClean (x:xs) (SimpleEnigma lr mr rr reflector (l,m,r)) = encodeChar lr mr rr (offset) [('.', '.')] reflector x: (encodeMessage (xs) (SimpleEnigma lr mr rr reflector (offset)))
                                                                 where offset = offsetUpdate rr mr (l,m,r)
  encodeClean (x:xs) (SteckeredEnigma lr mr rr reflector (l,m,r) stecker) = encodeChar lr mr rr (offset) stecker reflector x : encodeMessage (xs) (SteckeredEnigma lr mr rr reflector (offset) stecker)
                                                                 where offset = offsetUpdate rr mr (l,m,r)

  --cleans the input string to eliminate any non-letter characters and convert and lower-case letters to upper-case letters.
  cleanInput :: String -> String
  cleanInput string = map toUpper (filter isLetter string)


  --encodes a character with the details of the enigma setup and the character to encode.
  encodeChar :: Rotor -> Rotor -> Rotor -> Offsets -> Stecker -> Reflector -> Char -> Char
  encodeChar lr mr rr (l,m,r) stecker reflector c = steckerCheck (encodeCharBackward rr r (encodeCharBackward mr m (encodeCharBackward lr l (reflect reflector
                                                   (encodeCharForward lr l (encodeCharForward mr m (encodeCharForward rr r (steckerCheck c stecker)))))))) stecker


  --'runs' a character through the rotors from right-left ensuring they are correctly linked to eachother.
  encodeCharForward :: Rotor -> Int -> Char -> Char
  encodeCharForward rotor n c = alphabet!!((alphaPos((fst rotor)!!((alphaPos c + n) `mod` 26)) - n) `mod` 26)


  --'runs' a character through the rotors from left-right ensuring they are correctly linked to eachother.
  encodeCharBackward :: Rotor -> Int -> Char -> Char
  encodeCharBackward rotor n c = alphabet!!((alphaPos((alphabet)!!((indexOfChar(fst rotor)(alphabet!!(((alphaPos c) + n) `mod` 26))) `mod` 26)) - n) `mod` 26)


  --finds the index of a given character in a list.
  indexOfChar :: [Char] -> Char -> Int
  indexOfChar list c = length (takeWhile (/=c) list)


  --Checks the inputed character to see if it's in the plugboard configuration for possible pairs, if it is, it is replaced 
  --with its assigned pair, otherwise it stays the same.
  steckerCheck :: Char -> Stecker -> Char
  steckerCheck c stecker | toChar (map (checkPair c) stecker) == '.' = c
                         | toChar (map (checkPair c) stecker) /= '.' = toChar (map (checkPair c) stecker)
      
                             
  --Returns the character that is paired with the input character, i.e. for 'A', the output is 'Y'.
  reflect :: Reflector -> Char -> Char
  reflect reflector c = toChar (map (checkPair c) reflector)
    

  --takes offsets and the two rotors with knock-ons that matter and adjusts the offsets accordingly.
  offsetUpdate :: Rotor -> Rotor -> Offsets -> Offsets
  offsetUpdate rotorR rotorM (l,m,r) | (r == (snd rotorR) && m == (snd rotorM)) = ((l+1) `mod` 26,(m+1) `mod` 26,(r+1) `mod` 26)
                                           | (r == (snd rotorR)) = (l,(m+1) `mod` 26,(r+1) `mod` 26)
                                           | otherwise = (l,m,(r+1) `mod` 26)


  --This function checks the given pair and character to see if it exists in the pair, if it does the paired letter is returned, else '.' is returned.
  checkPair :: Char -> (Char, Char) -> Char
  checkPair c (a,b) | c == a = b
                    | c == b = a
                    | (c /= b && c /= a) = '.'

  --Converts the String combination of a letter and '.' into a single character, which is the letter.
  toChar :: [Char] -> Char
  toChar string | null (filter(/= '.') string) == False = head(filter (/= '.') string)
                | null (filter(/= '.') string) == True = '.'


{- Part 2: Finding the Longest Menu -}

  type Menu = [Int]
  type Crib = [(Char, Char)]
  --(Plaintext, Cypher)


  --A function that takes a crib and returns the longest menu that can be found in the crib.
  --If no menu can be found, the function returns an empty list.
  -- longestMenu :: Crib -> Menu
  -- longestMenu _ = []
  -- longestMenu cribCypher = maximum (allMenus crib cypher)
  --                     where cypher = map fst cribCypher
  --                           crib = map snd cribCypher


  -- --A function that takes a crib and returns a list of all the menus that can be found in the crib.
  -- --If no menu can be found, the function returns an empty list.
  -- allMenus :: Char -> Char -> [Menu]
  -- allMenus _ _ = []
  -- allMenus crib cypher = [menu crib cypher i | i <- [0..(length crib)-1]]

  -- --A function that works out the menu of a given cib and cypher.
  -- menu :: Char -> Char -> Int -> Menu
  -- menu _ _ _ = []
  -- menu crib cypher i = [length (takeWhile (matchPair crib cypher) (drop i crib))]


  -- matchPair :: Char -> Char -> Bool
  -- matchPair a c | a == c = True
  --               | otherwise = False

{- Part 3: Simulating the Bombe -}
  
--A function that takes a crib and returns a list of all the possible Enigma settings that could have been used to encrypt the crib.
--If no settings can be found, the function returns an empty list.
  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma _ = Nothing


  

{- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
        --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  alphabet=("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)


  {- the standard Enigma reflector (Reflector B)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
  -}
  reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]

  {- alphaPos: given an uppercase letter, returns its index in the alphabet
     ('A' = position 0; 'Z' = position 25)
   -}
  alphaPos :: Char -> Int
  alphaPos c = (ord c) - ord 'A'
