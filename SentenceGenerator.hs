module SentenceGenerator where 
 import Data.Char as Char
 
 data SentencePart = NonAdverb (String,String,String) | Adverb (String,String) 
                        deriving (Show,Eq)
                    
 getWord::SentencePart -> String
 getWord (NonAdverb (x,_,_)) = x 
 getWord (Adverb (x,_)) = x

 getPartLabel :: SentencePart -> String 
 getPartLabel (NonAdverb (_,x,_)) = x
 getPartLabel (Adverb _) = "Д"

 -- filter words (represented as SentenceParts) by their sentence part label
 getSPByLabel :: [SentencePart] -> String -> [SentencePart]
 getSPByLabel lst label = filter (\x -> getPartLabel x == label) lst

 getGenderOrForm :: SentencePart -> String
 getGenderOrForm (NonAdverb (_,_,x)) = x

 haveSameGender:: SentencePart -> SentencePart -> Bool
 haveSameGender adj sub = (getGenderOrForm adj == getGenderOrForm sub)

 {-capitalize the first letter of the word so that 
  it can be used for first word in a sentence -}
 makeValidFirstWord :: SentencePart -> String
 makeValidFirstWord (NonAdverb ((x:xs),_,_)) = Char.toUpper x : xs

 isValidSubjectVerbPair :: SentencePart -> SentencePart -> Bool
 isValidSubjectVerbPair sub verb = (getGenderOrForm sub == getGenderOrForm verb)  ||
                             ((getGenderOrForm sub) `elem` singulars && getGenderOrForm verb == singular)
            where singulars = ["м","ж","ср"]
                  singular = "ед"

 
 

 makeSentence::[SentencePart]-> [String]
 makeSentence sentenceParts = [ makeValidFirstWord adjective ++ " "++ getWord subject ++ " "++ getWord verb ++ " " ++ getWord adverb ++ "."
                                | adjective <- getSPByLabel sentenceParts "О" , 
                                  subject <- getSPByLabel sentenceParts "П" ,
                                  verb <- getSPByLabel sentenceParts "С" ,
                                  adverb <- getSPByLabel sentenceParts "Д",
                                  haveSameGender adjective subject,
                                  isValidSubjectVerbPair subject verb]

--  getRandomSentence::[SentencePart]-> [String]
--  getRandomSentence sentenceParts n = take n (makeSentence sentenceParts) 

 
        