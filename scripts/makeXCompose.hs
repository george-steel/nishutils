-- Outputs an XCompose fiel for use with syllabics kwyboards
-- Uses the keysym range 0xe0xx (presently unallocated) for dead keys to go with the XCompose file.
-- The lower nibble reperesents the consonants (in the order null,p,t,k,c,m,n,s,sh,y,l,r,th,v)
-- adding 16 for right w-dot and 32 for left w-dot

import Control.Applicative
import UnFierro
import Numeric
import Data.Char
import Control.Monad

cdead =  [("",     0xe000)
         ,("p",    0xe001)
         ,("t",    0xe002)
         ,("k",    0xe003)
         ,("c",    0xe004)
         ,("m",    0xe005)
         ,("n",    0xe006)
         ,("s",    0xe007)
         ,("sh",   0xe008)
         ,("y",    0xe009)
         ,("l",    0xe00a)
         ,("r",    0xe00b)
         ,("th",   0xe00c)
         ,("v",    0xe00d)]

cnw = ["", "p", "t", "k", "c", "m", "n", "s", "sh", "y", "l", "r", "th", "v"]

vowels = ["e","i","ii","o","oo","a","aa"]
vnl = ["e","i","o","a"]

ucode c = "<U" ++ showHex (ord (head c)) ">"
keyhex i = "<0x" ++ showHex i ">"

chrlw c = [chr (ord (head c) - 1)]

sylline (c,dcode) v = if (c == "th" || c == "v" || c == "r")
                         then           keyhex dcode       ++ " " ++ ucode (unfierro v) ++ " : \"" ++ unfierro (c ++ v) ++ "\"\n"
                         else unlines [ keyhex dcode       ++ " " ++ ucode (unfierro v) ++ " : \"" ++ unfierro (c ++ v) ++ "\""
                                      , keyhex (dcode+16)  ++ " " ++ ucode (unfierro v) ++ " : \"" ++ unfierro (c ++ "w" ++ v) ++ "\""
                                      , keyhex (dcode+32)  ++ " " ++ ucode (unfierro v) ++ " : \"" ++ chrlw (unfierro (c ++ "w" ++ v)) ++ "\"" ] 

deadline c "e" = if  (c == "th" || c == "v" || c == "r")
                     then ""
                     else unlines  [ "<0xe010> "                  ++ ucode (unfierro (c ++ "e")) ++ " : \""  ++ unfierro (c ++ "we") ++ "\""
                                   , "<0xe020> "                  ++ ucode (unfierro (c ++ "e")) ++ " : \""  ++ chrlw (unfierro (c ++ "we")) ++ "\"" ]
deadline c v   = if  (c == "th" || c == "v" || c == "r")
                     then          ("<dead_abovedot> "            ++ ucode (unfierro (c ++ v)) ++ " : \""  ++ unfierro (c ++ v ++ v) ++ "\"\n")
                     else unlines  [ "<dead_abovedot> "           ++ ucode (unfierro (c ++ v)) ++ " : \""  ++ unfierro (c ++ v ++ v) ++ "\""
                                   , "<0xe010> "                  ++ ucode (unfierro (c ++ v)) ++ " : \""  ++ unfierro (c ++ "w" ++ v) ++ "\""
                                   , "<dead_abovedot> <0xe010> "  ++ ucode (unfierro (c ++ v)) ++ " : \""  ++ unfierro (c ++ "w" ++ v ++ v) ++ "\""
                                   , "<0xe010> <dead_abovedot> "  ++ ucode (unfierro (c ++ v)) ++ " : \""  ++ unfierro (c ++ "w" ++ v ++ v) ++ "\"" 
                                   , "<0xe020> "                  ++ ucode (unfierro (c ++ v)) ++ " : \""  ++ chrlw (unfierro (c ++ "w" ++ v)) ++ "\""
                                   , "<dead_abovedot> <0xe020> "  ++ ucode (unfierro (c ++ v)) ++ " : \""  ++ chrlw (unfierro (c ++ "w" ++ v ++ v)) ++ "\""
                                   , "<0xe020> <dead_abovedot> "  ++ ucode (unfierro (c ++ v)) ++ " : \""  ++ chrlw (unfierro (c ++ "w" ++ v ++ v)) ++ "\"" ]

syllblock = join (sylline <$> cdead <*> vowels)
deadblock = join (deadline <$> cnw <*> vnl)

main = putStrLn "include \"%L\"\n" >> putStrLn syllblock >> putStrLn deadblock
