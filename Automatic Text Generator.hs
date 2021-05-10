import DataFile

--wordToken done.
wordToken (x) = words (wordTokenHelper x)

wordTokenHelper  []=[]
wordTokenHelper (x:xs) = if ((head xs) `elem` punct) then [(x)] ++" " ++wordTokenHelper xs
              else [x] ++ wordTokenHelper xs


-- wordTokenList done.
wordTokenList []=[]
wordTokenList (x:xs) = wordToken x ++ wordTokenList xs


--uniqueBigrams done.
uniqueBigrams x =removeduplicate((uniqueBigramsHelper1 x))

uniqueBigramsHelper1 [x]=[]
uniqueBigramsHelper1 (x:xs)= (x,(head xs)):uniqueBigrams xs

removeduplicate [] = []
removeduplicate (x:xs)   | x `elem` xs   = removeduplicate xs
                              | otherwise     = x : removeduplicate xs


--uniqueTrigrams done.
uniqueTrigrams x = removeduplicate( uniqueTrigramsHelper1 x)

uniqueTrigramsHelper1 [x]=[]
uniqueTrigramsHelper1 (x:xs) | length xs ==1 =[]
                  | otherwise =(x,(head xs), (uniqueTrigramsHelper2 xs)): uniqueTrigramsHelper1 xs

uniqueTrigramsHelper2 (x:xs) = (head xs)


--bigramsFreq

bigramsFreq :: Num a => [String] -> [((String,String),a)]

bigramsFreqHelper:: Num a => [(String,String)] -> [(String,String)] -> [((String,String),a)]
numInBigrams :: Num a => (String, String) -> [(String, String)] -> a -> a

allBigrams :: [String] -> [(String,String)]
allBigramsHelper:: [String] -> [(String,String)] -> [(String,String)] 


bigramsFreq list = bigramsFreqHelper (uniqueBigrams list) (allBigrams list)

allBigrams list = allBigramsHelper list []

allBigramsHelper (h:l:t) list | t==[] = (list++[(h,l)])  
							  | otherwise = allBigramsHelper (l:t) (list++[(h,l)])  

bigramsFreqHelper (x:xs) list | xs==[] = [((x),numInBigrams x list 0)]
							  |otherwise = [((x),numInBigrams x list 0)]++bigramsFreqHelper xs list	 
								 
numInBigrams (h,l) ((x,y):t) n | t==[] && h==x&&l==y = n+1
							   | t==[] = n
							   | h==x&&l==y = numInBigrams (h,l) t n+1								 
							   | otherwise = numInBigrams (h,l) t n
							 


--trigramsFreq

trigramsFreq :: Num a => [String] -> [((String,String,String),a)]

trigramsFreqHelper:: Num a => [(String,String,String)] -> [(String,String,String)] -> [((String,String,String),a)]
numInTrigrams :: Num a => (String, String,String) -> [(String,String, String)] -> a -> a

allTrigrams :: [String] -> [(String,String,String)]
allTrigramsHelper:: [String] -> [(String,String,String)] -> [(String,String,String)] 


trigramsFreq list = trigramsFreqHelper (uniqueTrigrams list) (allTrigrams list)

allTrigrams list = allTrigramsHelper list []

allTrigramsHelper (h:l:m:t) list | t==[] = (list++[(h,l,m)])  
							  | otherwise = allTrigramsHelper (l:m:t) (list++[(h,l,m)])  

trigramsFreqHelper (x:xs) list | xs==[] = [((x),numInTrigrams x list 0)]
							  |otherwise = [((x),numInTrigrams x list 0)]++trigramsFreqHelper xs list	 
								 
numInTrigrams (h,l,m) ((x,y,z):t) n | t==[] && h==x&&l==y &&m==z = n+1
							   | t==[] = n
							   | h==x&&l==y&&m==z = numInTrigrams (h,l,m) t n+1								 
							   | otherwise = numInTrigrams (h,l,m) t n



--getFreq done.
getFreq _ [] =0
getFreq y ((x1,x2):xs) | y == x1 =x2
                       | otherwise = getFreq y xs


-- generateOneProb done.
generateOneProb _ []=0
generateOneProb ((x1,x2,x3),y1) (((c1,c2),y2):xs) | (x1 ==c1) && (x2==c2) = y1/y2
                                                | otherwise = generateOneProb ((x1,x2,x3),y1) xs


-- genProbPairs done.
genProbPairs [] _ =[]
genProbPairs (((x1,x2,x3),y1):xs) c = ((x1,x2,x3),(generateOneProb ((x1,x2,x3),y1) c)): genProbPairs xs c


--- generateNextWord done.
generateNextWord w p = generateNextWordHelper1 (generateNextWordHelper2 w p) (randomZeroToX ((length (generateNextWordHelper2 w p))-1)) 0

generateNextWordHelper2 _ [] =[]
generateNextWordHelper2 ([w1,w2]) (((x1,x2,x3),y1):xs) | (w1 == x1) && (w2 == x2) && (y1 > 0.03) = x3:generateNextWordHelper2 ([w1,w2]) xs
                                                   |otherwise= generateNextWordHelper2 ([w1,w2]) xs
--( x3 :generateNextWordHelper2 ([w1,w2]) xs)

generateNextWordHelper1 [] _ _=error ("Sorry, it is not possible to infer from current database")
generateNextWordHelper1 (x:xs) n1 n2 | (n1 ==n2)= x
                                   | otherwise =generateNextWordHelper1 xs n1 (n2+1)


-- generateText done.

generateText w 0 =w
generateText w n = generateText (w++" "++x) (n-1) 
                         where
                                x= generateNextWord (lastTwo (words w)) (genProbPairs (trigramsFreq (wordTokenList docs)) (bigramsFreq (wordTokenList docs)))


lastTwo ([x,y])= ([x,y])
lastTwo (x:xs)= lastTwo xs


-- Evaluation Function Done 

sentToken:: [Char] -> [String]
sentTokenhelperfirst:: String -> Bool -> String
sentTokenhelperlast:: String -> Bool -> String
sentTokenlisthelper:: String -> [String] -> [String]

sentToken s = removeEmpty (sentTokenlisthelper s [])


sentTokenlisthelper text list | text/="" = list++sentTokenlisthelper (sentTokenhelperlast text False) ([sentTokenhelperfirst text False])
							  | text=="" = list


sentTokenhelperfirst (h:t) b| h==' ' && b==False = [] ++sentTokenhelperfirst (t) False
							|(occursIno h ['.','!','?'])==False && t/=[] = [h]++sentTokenhelperfirst (t) True
							|(occursIno h ['.','!','?'])==True&& b==False = []
							|(occursIno h ['.','!','?'])==True&& b ==True = [h]	
							| t==[] = [h]
							| otherwise = []
					 
sentTokenhelperlast (h:t) b 
						    | (occursIno h ['.','!','?'])==False && t/=[] = sentTokenhelperlast t True
						    | (occursIno h ['.','!','?'])==True && b== False = t
						    | (occursIno h ['.','!','?'])==True && b== True = (h:t)
						    | otherwise = t


occursIno x [] = False
occursIno a (x:xs) | a == x = True
				  |otherwise = occursIno a xs

removeEmpty [] = []

removeEmpty (x:xs) | x==[] = removeEmpty xs
				   | x/=[] = [x]++ removeEmpty xs

















 

 




