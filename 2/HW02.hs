{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
--
-- Example: formableBy "fun" ['x','n','i','f','u','e','l'] == True
-- Example: formableBy "haskell" [’k’,’l’,’e’,’h’,’a’,’l’,’s’] == True
-- Example: formableBy "haskell" [’k’,’l’,’e’,’h’,’a’,’y’,’s’] == False

formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (first_letter:other_letters) letter_bank
  | (first_letter `elem` letter_bank) = (formableBy other_letters (delete first_letter letter_bank))
  | otherwise = False



-- Example: wordsFrom [’a’,’b’,’c’,’d’] == ["ab","ad","ba","bad","cab","cad","dab"]
-- Example: wordsFrom [’h’,’e’,’l’,’l’,’o’] == [ "eh","el","ell","he","hell","hello","helo", "ho","hoe","hole","lo","oe","oh","ole" ]

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords



-- Example: wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "care" == True
-- Example: wordFitsTemplate "??r?" ['c','x','e','w','b','c','l'] "care" == False
-- Example: wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "car" == False
-- Example: wordFitsTemplate "let" ['x','x'] "let" == True

wordFitsTemplate :: Template -> Hand -> String -> Bool

wordFitsTemplate template hand word
  | template == word = True

wordFitsTemplate [] _ _ = False

wordFitsTemplate template hand [] = False

wordFitsTemplate (first_letter_of_the_template:template) hand (first_lettter_of_the_word:word)
  | first_letter_of_the_template == first_lettter_of_the_word = wordFitsTemplate template hand word
  | first_letter_of_the_template_is_a_wildcard && first_lettter_of_the_word_found_in_hand = wordFitsTemplate template (delete first_lettter_of_the_word hand) word
  | otherwise = False
  where
    first_lettter_of_the_word_found_in_hand = first_lettter_of_the_word `elem` hand
    first_letter_of_the_template_is_a_wildcard = first_letter_of_the_template == '?'



--  Example: wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l'] ==["acre","bare","carb","care","carl","earl"]

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate template hand = filter (\w -> wordFitsTemplate template hand w) allWords

--map (\w -> wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] w) (wordsFrom ['c','x','e','a','b','c','l'])
--map (\w -> w) (wordsFrom ['c','x','e','a','b','c','l'])
