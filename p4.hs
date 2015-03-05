import Debug.Trace

data Color = Indigo | Cyan --deriving Eq
data Cell = Rien | Plein Color --deriving Eq
type Column = [Cell]
type Grid = [Column]
data Int6 = Zero | Un | Deux | Trois | Quatre | Cinq | Six

val :: Int6 -> Int
val Zero = 0
val Un = 1
val Deux = 2
val Trois = 3
val Quatre = 4
val Cinq = 5
val Six = 6

initialCol = replicate 6 Rien
initialGrid = replicate 7 initialCol

instance Show Cell where 
	show Rien			= " . "
	show (Plein Indigo)	= " I "
	show (Plein Cyan)   = " C "  
--	show (Cell a) = show Color a

instance Eq Cell where
	Rien == Rien = True
	(Plein Indigo) == (Plein Indigo) = True
	(Plein Cyan) == (Plein Cyan) = True
	_ == _ = False

show2D :: Grid -> String
show2D [] = ""
--show2D [x] = concat [show c | c <- x]
show2D [x] = concatMap show x
show2D (x:xs) = (show2D [x]) ++ "\n" ++ (show2D xs)

addToken :: Column -> Column -> Color -> Column
addToken x0 [] c = x0
addToken x0 (Rien:xs) c = concat [x0, (Plein c:xs)]
addToken x0 (x:xs) c = addToken (x0++[x]) xs c

after :: Grid -> Int -> Grid
after l 0 = l
after (x:xs) n = after xs (n-1) 

play :: Grid -> Grid -> Color -> Int6 -> Int -> Grid
play g0 g c indexDest indexCurr = if (val indexDest) == indexCurr 
	then concat [g0, [(addToken [] (g!!(val indexDest)) c)] , after g (1+(val indexDest)) ]
	else play (g0++[g!!indexCurr]) g c indexDest (indexCurr+1) 

playprop :: [(Column,Int)] -> Grid -> Color -> Int -> Int -> Grid
playprop zg g c i icurr = if snd(zg!!icurr) == i 
	then replace i (addToken [] (g!!i) c ) g else playprop zg g c i (icurr+1)

replace n item lst = a ++ (item:b) where (a,(_:b)) = splitAt n lst

summerize':: Column -> Cell -> Int -> [(Int,Cell)] -> [(Int,Cell)]
summerize' (cCurr:xs) cBase i lst = if cCurr == cBase 
	then summerize' xs cBase (i+1) lst
	else summerize' xs cCurr 1 ((i,cBase):lst)
summerize' [] cBase i lst = ((i,cBase):lst)

summerize :: Column -> [(Int,Cell)]
summerize c = summerize' c (c!!0) 0 []

plusRien :: Column -> Int -> Bool -> Column
plusRien c 0 _ = c
plusRien c i reverse = if reverse then plusRien ([Rien]++c) (i-1) reverse
							else plusRien (c++[Rien]) (i-1) reverse

diagonalize :: Grid -> Int -> Grid
diagonalize [] _ = []
diagonalize g i = if i == (length g) then g 
	else diagonalize (replace i (plusRien (plusRien(g!!i) i True) ((length g)-i) False) g) (i+1) 

--datum = show (Plein Indigo)
--datom = show2D [initialCol,[Plein Indigo]]
--datim = addToken [] [Plein Indigo,Plein Indigo] Cyan
--datem = play [] initialGrid Cyan Deux 0
--datom = play [] datem Cyan Deux 0
datem = playprop (zip initialGrid [0..6]) initialGrid Cyan 2 0
datym = playprop (zip datem [0..6]) datem Indigo 2 0
datim = diagonalize datym 0
datum = summerize (datym!!2)


main = do {(putStr.show2D) datim}

