{-# LANGUAGE
    LambdaCase,
    ExistentialQuantification
    #-}

module SuperSimpleFormatter where

import Control.Arrow

data Token = Replace | Literal String

breakUp :: String -> [Token]
breakUp s = snd $ break (s, [])
            where break ([], toks) = ([], toks)
                  break (('_':'_':s), toks) = break (s, addChar '_' toks)
                  break (('_':s), toks) = break (s, Replace:toks)
                  break ((c:s), toks) = break (s, addChar c toks)
                  
                  addChar c (Literal s:toks) = Literal (c:s) : toks
                  addChar c toks = Literal [c] : toks

rejoin :: [Token] -> String
rejoin = concatMap (\case Literal s -> s; Replace -> "_")

-- I *could* use an existentially-quantified box, but that's not really necessary.
-- if you want to do that, you can call 'show on the arguments yourself.
replace :: [Token] -> [String] -> String
replace [] _ = ""
replace (Literal s:toks) xs = s ++ replace toks xs
replace toks [] = rejoin toks
replace (Replace:toks) (x:xs) =  x ++ replace toks xs

(%) :: String -> [String] -> String
(%) = curry (first breakUp >>> uncurry replace)

data ShowList = Nil | forall s. Show s => s ::$ ShowList

toList :: ShowList -> [String]
toList Nil = []
toList (s ::$ l) = show s : toList l