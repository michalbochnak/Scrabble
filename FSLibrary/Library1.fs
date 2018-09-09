//
// My F# library for scrabble word generation.
//
// Michal Bochnak
// Netid: mbochn2
// U. of Illinois, Chicago
// CS 341, Fall 2017
// Project #05, Scrabble word generator
//

// Notes:
//  1. Make sure sort works for all of the outputs
//  2. Convert letters to lowercase and/or dictionary too


module MyLibrary

open System.Security.AccessControl

#light


//
// explode a string into a list of characters.
// Example: "cat" -> ['c'; 'a'; 't']
//
let private explode2c(s:string) =
  [ for c in s -> c ]


//
// implode a list L of characters back into a string.
// Example: implode ['c'; 'a'; 't'] -> "cat"
//
let private implode L =
  let sb = System.Text.StringBuilder()
  let ignore = List.map (fun c -> sb.Append (c:char)) L
  sb.ToString()


//
// Initialize:
//
// This function is called ONCE at program startup to initialize any
// data structures in the library.  We use this function to input the
// Scrabble dictionary and build a list of legal Scrabble words.
//
let mutable private WordList = []

let Initialize folderPath =
  let alphabetical = System.IO.Path.Combine(folderPath, "alphabetical.txt")
  WordList <- [ for line in System.IO.File.ReadAllLines(alphabetical) -> line ]
  printfn "%A" (List.length WordList)


//
// removeLetter
// 
// removes the given letter from the word
//
let rec removeLetter letter word acc =
    match word with
    | [] -> acc
    | hd::tl -> if (hd = letter)  // match 
                then acc @ tl     // return acc + tl (hd ommited)
                else removeLetter letter tl (hd :: acc)
                      

//
// contains
// 
// checks if letter is in the word
// returns true if so, false otherwise
//
let rec contains letter word = 
    match word with
    | [] -> false
    | hd::tl -> if (hd = letter)  // found
                then true
                else
                contains letter tl
              
              
//
// canBeFormed
// 
// Check if the given word can be spelled using given letters
// retuns true if so, false is returned otherwise
//
let rec canBeFormed word letters =
    match word with
    | [] -> true
    | hd::tl -> if (contains hd letters)  // letter present, remove it
                then canBeFormed tl (removeLetter hd letters [])
                else false


//
// possibleWords
//
// Finds all Scrabble words in the Scrabble dictionary that can be 
// spelled with the given letters.  The words are returned as a list
// in alphabetical order.
//
// Example:  letters = "tca" returns the list
//   ["act"; "at"; "cat"; "ta"]
//
let possibleWords letters =
    // collect words with correct length
    let possibleWords = List.filter ( fun w -> (String.length w <= String.length letters)) WordList
    // explode them to list of chars
    let expPossibleWords = [for w in possibleWords -> (explode2c w)]
    // collect words that can be formed with given letters
    let validWords = List.filter (fun word -> (canBeFormed word (explode2c letters))) expPossibleWords
    // sort the list
    let sortedValidWords = List.sortBy (fun x -> x) validWords
    // return list imploded into list of strings
    [for w in sortedValidWords -> implode w]
  
//
// letterScore
//
// returns the value score for each letter
//
let letterScore letter =
    match letter with
    | 'a' -> 1
    | 'b' -> 3
    | 'c' -> 3
    | 'd' -> 2
    | 'e' -> 1
    | 'f' -> 4
    | 'g' -> 2
    | 'h' -> 4
    | 'i' -> 1
    | 'j' -> 8
    | 'k' -> 5
    | 'l' -> 1
    | 'm' -> 3
    | 'n' -> 1
    | 'o' -> 1
    | 'p' -> 3
    | 'q' -> 10
    | 'r' -> 1
    | 's' -> 1
    | 't' -> 1
    | 'u' -> 1
    | 'v' -> 4
    | 'w' -> 4
    | 'x' -> 8
    | 'y' -> 4
    | 'z' -> 10    


//
// computeScore
//
// calculates the score for the given word recursively
//
let rec computeScore word =
    match word with
    | [] -> 0
    | hd::tl -> (letterScore hd) + (computeScore tl)
    

//
// calcScores
//
// calculates the score for every word in the given list
//
let calcScores words =
    [ for w in words -> computeScore w ] 


//
// mergeToTuples
//
// merges two lists into one list of tuples
// Assumes both lists are same size
//
let rec mergeToTuples validWords wordScores acc=
    match validWords, wordScores with
    | [], [] -> acc
    | hd1::tl1, hd2::tl2 -> mergeToTuples tl1 tl2 ((hd1, hd2) :: acc)
  

//
// wordsWithScores
//
// Finds all Scrabble words in the Scrabble dictionary that can be 
// spelled with the given letters.  The words are then "scored"
// based on the value of each letter, and the results returned as
// a list of tuples in the form (word, score).  The list is ordered
// in descending order by score; if 2 words have the same score,
// they are ordered in alphabetical order.
//
// Example:  letters = "tca" returns the list
//   [("act",5); ("cat",5); ("at",2); ("ta",2)]
//
let wordsWithScores letters =
  // get list of valid words
  let validWords = possibleWords letters
  // get list of the scores for valid words
  let wordScores = calcScores [ for w in validWords -> explode2c w ]
  // merge valid words and scores into list of tuples
  let tuples = mergeToTuples validWords wordScores []
  // sort the list with tuples
  let sortedAlf = List.sortBy (fun (a, _) -> a) tuples
  let sorted = List.sortBy (fun (_, b) -> -b) sortedAlf
  sorted


//
// patternWords
//
// Checks which words in given list matches the pattern
// while using only provided letters
// Returns the list with matched words
// Assumes all three lists are the same length
//
let rec patternWord pattern word letters =
    match pattern, word with
    | [], [] -> true
    | hd1::tl1, hd2::tl2 -> // 'star' than can be replaced by letter from available set
                            if ((hd1 = '*') && (contains hd2 letters)) 
                            then (patternWord tl1 tl2 (removeLetter hd2 letters []))
                            // letter matches
                            elif (hd1 = hd2)
                            then (patternWord tl1 tl2 (removeLetter hd2 letters []))
                            // letters do not match and 'star' can not
                            // be replaced with letter form set
                            else 
                                false

                                
//
// wordsThatFitPattern
//
// Finds all Scrabble words in the Scrabble dictionary that can be 
// spelled with the given letters + the letters in the pattern, such
// that those words all fit into the pattern.  The results are 
// returned as a list of tuples (word, score), in descending order by
// score (with secondary sort on the word in alphabetical order).
//
// Example:  letters = "tca" and pattern = "e**h" returns the list
//   [("each",9); ("etch",9); ("eath",7)]
//5
let wordsThatFitPattern letters pattern = 
  // collect possible words with correct length
  let possibleWords = List.filter ( fun w -> (String.length w = String.length pattern)) WordList
  // explode them to char list
  let expPossibleWords = [for w in possibleWords -> (explode2c w)]
  // collect the words that fits pattern
  let patternWords = List.filter (fun word -> (patternWord (explode2c pattern) word (explode2c letters))) expPossibleWords
  // implode words into string list
  let impPatternWords = [ for w in patternWords -> implode w ]
  // compute the scores for the words
  let wordScores = calcScores [ for w in patternWords -> w ]
  // combine pattern words with their scores into one list of tuples
  let tuples = mergeToTuples impPatternWords wordScores []
  // sort the list
  let sortedAlf = List.sortBy (fun (a, _) -> a) tuples
  let sorted = List.sortBy (fun (_, b) -> -b) sortedAlf
  sorted