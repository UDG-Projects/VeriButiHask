-- Imports
import System.Random

-- Constants
cartes1 = [
  [Carta Manilla Bastos, Carta Vuit Bastos, Carta Tres Espases, Carta As Copes, Carta Quatre Bastos, Carta Cavall Espases, Carta Set Copes, Carta As Oros, Carta Cinc Bastos, Carta Sota Copes, Carta Quatre Espases, Carta Set Bastos],
  [Carta Sota Bastos, Carta Cavall Bastos, Carta Manilla Espases, Carta Vuit Copes, Carta Cinc Oros, Carta Vuit Espases, Carta Manilla Copes, Carta Sis Oros, Carta Sota Oros, Carta Cinc Copes, Carta Set Oros, Carta Quatre Copes],
  [Carta As Bastos, Carta Dos Oros, Carta Rei Espases, Carta Cavall Copes, Carta Vuit Oros, Carta As Espases, Carta Rei Copes, Carta Rei Oros, Carta Tres Copes, Carta Sis Copes, Carta Sis Espases, Carta Cinc Espases],
  [Carta Dos Bastos, Carta Tres Bastos, Carta Dos Espases, Carta Dos Copes, Carta Sis Bastos, Carta Set Espases, Carta Tres Oros, Carta Quatre Oros, Carta Rei Bastos, Carta Cavall Oros, Carta Manilla Oros, Carta Sota Espases]]

cartesMod = [
  [Carta Manilla Bastos, Carta Vuit Bastos, Carta Tres Espases, Carta As Copes, Carta Quatre Bastos, Carta Cavall Espases, Carta Set Copes, Carta Manilla Copes, Carta Cinc Bastos, Carta Sota Copes, Carta Quatre Espases, Carta Set Bastos],
  [Carta Sota Bastos, Carta Cavall Bastos, Carta Manilla Espases, Carta Vuit Copes, Carta Cinc Oros, Carta Vuit Espases, Carta As Oros, Carta Sis Oros, Carta Sota Oros, Carta Cinc Copes, Carta Set Oros, Carta Quatre Copes],
  [Carta As Bastos, Carta Dos Oros, Carta Rei Espases, Carta Cavall Copes, Carta Vuit Oros, Carta As Espases, Carta Rei Copes, Carta Rei Oros, Carta Tres Copes, Carta Sis Copes, Carta Sis Espases, Carta Cinc Espases],
  [Carta Dos Bastos, Carta Tres Bastos, Carta Dos Espases, Carta Dos Copes, Carta Sis Bastos, Carta Set Espases, Carta Tres Oros, Carta Quatre Oros, Carta Rei Bastos, Carta Cavall Oros, Carta Manilla Oros, Carta Sota Espases]]


partida1=[Carta Manilla Bastos, Carta Sota Bastos,  Carta As Bastos, Carta Dos Bastos,
 Carta Vuit Bastos, Carta Cavall Bastos, Carta Dos Oros, Carta Tres Bastos,
 Carta Rei Espases, Carta Dos Espases, Carta Tres Espases, Carta Manilla Espases,
 Carta Vuit Copes, Carta Cavall Copes, Carta Dos Copes, Carta As Copes,
 Carta Quatre Bastos, Carta Cinc Oros, Carta Vuit Oros, Carta Sis Bastos,
 Carta As Espases, Carta Set Espases, Carta Cavall Espases, Carta Vuit Espases,
 Carta Rei Copes, Carta Tres Oros, Carta Set Copes, Carta Manilla Copes,
 Carta Quatre Oros, Carta As Oros, Carta Sis Oros, Carta Rei Oros,
 Carta Cinc Bastos, Carta Sota Oros, Carta Tres Copes, Carta Rei Bastos,
 Carta Cinc Copes, Carta Sis Copes, Carta Cavall Oros, Carta Sota Copes,
 Carta Manilla Oros, Carta Quatre Espases, Carta Set Oros, Carta Sis Espases,
 Carta Sota Espases, Carta Set Bastos, Carta Quatre Copes, Carta Cinc Espases]

basa1=[Carta Sota Bastos, Carta As Bastos, Carta Manilla Bastos, Carta Dos Bastos]

-- Random seed
-- newRand = newStdGen

-- Tipus
data Pal = Bastos | Copes | Oros | Espases deriving (Show)
instance Eq Pal where
  Bastos == Bastos    = True
  Oros == Oros        = True
  Copes == Copes      = True
  Espases == Espases  = True
  _ == _              = False


data Trumfu = Butifarra | Pal Pal
instance Eq Trumfu where
  Butifarra == Butifarra  = True
  Pal p1 == Pal p2        = p1 == p2
  _ == _                  = False
instance Show Trumfu where
  show (Butifarra) = "Butifarra"
  show (Pal pal)   = show pal


data TipusCarta = Dos | Tres | Quatre | Cinc | Sis | Set | Vuit | Sota | Cavall | Rei | As | Manilla deriving (Show, Ord)
instance Eq TipusCarta where
  Dos == Dos          = True
  Tres == Tres        = True
  Quatre ==  Quatre   = True
  Cinc == Cinc        = True
  Sis == Sis          = True
  Set == Set          = True
  Vuit == Vuit        = True
  Sota == Sota        = True
  Cavall == Cavall    = True
  Rei == Rei          = True
  As ==  As           = True
  Manilla == Manilla  = True
  _ == _              = False

data Carta = Carta TipusCarta Pal
instance Eq Carta where
  Carta te pe == Carta td pd = (te == td) && (pe == pd)
instance Show Carta where
  show (Carta a Bastos) = show a ++ " de " ++ show Bastos
  show (Carta a Copes) = show a ++ " de " ++ show Copes
  show (Carta a b)   = show a ++ " d'" ++ show b
instance Ord Carta where
  (Carta te pe) <= (Carta td pd) = te <= td

-- Funcions -------------------------------------------------------------------------------

-- Pre :
-- Post :
valor :: Carta -> Int
valor (Carta tc _)
  | tc == Manilla = 5
  | tc == As      = 4
  | tc == Rei     = 3
  | tc == Cavall  = 2
  | tc == Sota    = 1
  | otherwise     = 0

-- Pre : 0 < [x && y] < 5
-- Post : [1-4] referent al proper a tirar.
quiSortira :: Int -> Int -> Int
quiSortira x y
  | modul == 0 = 4
  | otherwise = modul
  where
    modul = ((mod) ((-) ((+) x y) 1) 4)

seguentJugador :: Int -> Int
seguentJugador jugador
  | jugador == 4 = 1
  | otherwise = jugador + 1

cartaJugadorBasa :: [Carta] -> Int -> Int -> Carta
cartaJugadorBasa (carta:pila) comenca jugador
  | comenca == jugador = carta
  | otherwise = cartaJugadorBasa pila (seguentJugador comenca) jugador

-- Pre : Mira si la primera carta mata a la segona
-- Post : Si la primera carta mata a la segona retorna true, false altrament, té en compte el trumfo de la partida.
mata :: Trumfu -> Carta -> Carta  -> Bool
mata trumfu (Carta tc1 pal1) (Carta tc2 pal2)
  | trumfu == Butifarra = (pal1 == pal2) && ((Carta tc1 pal1) < (Carta tc2 pal2))
  | otherwise = ((pal1 == pal2) && (Carta tc1 pal1) < (Carta tc2 pal2)) || ((pal1 /= pal2) && ((\(Pal p)->p == pal2) trumfu))

-- Pre : Filtra les cartes d'un pal en concret
-- Post : Cartes de pal PalDemanat o llista buida.
cartesPal :: [Carta] -> Pal -> [Carta]
cartesPal ll palDemanat = filter (\(Carta t p)->p == palDemanat) ll

-- Pre : Llista != []
-- Post : Retorna el pal guanyador donat llista de cartes i trumfo
palGuanyadorBasa :: [Carta] -> Trumfu -> Pal
palGuanyadorBasa [] trumfo = error "No em pots passar una llista buida animal! "
palGuanyadorBasa ll Butifarra = head [ pal | (Carta t pal) <-ll ]
palGuanyadorBasa ll (Pal trumfu) = if length (cartesPal ll trumfu) > 0 then trumfu else head [ pal | (Carta t pal) <-ll ]

-- Pre : Llista != []
-- Post : La carta i la posició guanyadora en forma de tupla
quiGuanya :: [Carta] -> Trumfu -> (Carta, Int)
quiGuanya [] trumfo = error "No em pots passar una llista buida Animal! "
quiGuanya ll trumfo =  (cartaGuanyadora, (head [index | (index, carta) <- zip [1..] ll, carta == cartaGuanyadora]))
  where
    palGuanyador = palGuanyadorBasa ll trumfo
    cartaGuanyadora = maximum (cartesPal ll palGuanyador)

-- Pre : Rep posició del que guanya actualment i la meva posició.
-- Post : Retorna cert si s'ha de matar fals altrament
saDeMatar :: Int -> Int -> Bool
saDeMatar posGuanya posMeu
  | posMeu - 2 > 0 = posMeu - 2 /= posGuanya
  | otherwise = True

selecciona :: Bool -> [Carta] -> [Carta] -> [Carta]
selecciona b l1 l2 = if b then l1 else l2

jugades :: [Carta] -> Trumfu -> [Carta] -> [Carta]
jugades cartesJugador _ [] = cartesJugador
jugades cartesJugador trumfu ll =
  if (\(Carta tc pal)->pal== palBasa) (fst guanyador) then
    if (length cartesJugadorPalBasa) > 0 then
      selecciona (esticObligatAMatar && (length (cartesJugadorMatenPalBasa) > 0)) cartesJugadorMatenPalBasa cartesJugadorPalBasa
    else
      selecciona ( esticObligatAMatar && ((length cartesJugadorMaten) > 0)) cartesJugadorMaten cartesJugador
  else
    if (length cartesJugadorPalBasa) > 0 then cartesJugadorPalBasa
    else
      selecciona (esticObligatAMatar && (length cartesJugadorMaten) > 0) cartesJugadorMaten cartesJugador
  where
    guanyador = quiGuanya ll trumfu
    esticObligatAMatar = saDeMatar (snd guanyador) ((length ll) + 1)
    palBasa = ((\(Carta tc pal)->pal) (head ll))
    cartesJugadorPalBasa = cartesPal cartesJugador palBasa
    cartesJugadorMaten = filter (mata trumfu (fst guanyador)) cartesJugador
    cartesJugadorMatenPalBasa = cartesPal cartesJugadorMaten palBasa

-- Pre :
-- Post :
extreu :: [[Carta]] -> [Carta] -> Int -> [[Carta]]
extreu mans basa jug = [filter (/=cartaJugadorBasa basa jug x) (mans!!(x - 1)) | x <- [1..4]]
-- extreu mans basa jug = [(filter (/=(cartaJugadorBasa basa jug 1)) (mans!!0)),
                        --(filter (/=(cartaJugadorBasa basa jug 2)) (mans!!1)),
                        --(filter (/=(cartaJugadorBasa basa jug 3)) (mans!!2)),
                        --(filter (/=(cartaJugadorBasa basa jug 4)) (mans!!3))]

-- Pre :
-- Post :
properATirar :: [Carta] -> Trumfu -> Int -> Int
properATirar basa trumfu jug = (quiSortira jug (snd (quiGuanya basa trumfu)))

-- Pre :
-- Post :
trampa :: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe ([Carta],Int, Int)
trampa _ _ [] _ = Nothing
trampa ll trumfu pila jug =
  if or [fst x | x<-hiHaTrampa] then
    Just (basa, (12 - (length (head ll))) + 1 , (head [snd x | x<-hiHaTrampa, (fst x)]) + 1)
    --Just ((fst ( jugadesGen!!1)),(snd (jugadesGen!!2)),1)
  else trampa (extreu ll basa jug) trumfu (drop 4 pila) (properATirar basa trumfu jug) --(quiSortira jug (snd (quiGuanya basa trumfu)))
  where
    -- Mirem que les cartes estiguin dintre de jugades
    basa = take 4 pila
    hiHaTrampa= [((notElem (pila!!n) (jugades (ll!!(mod (jug + (n-1)) 4)) trumfu (take n pila))), (mod (jug +(n-1)) 4)) | n <-[0..3] ]
    --jugadesGen= [((jugades (ll!!(mod (jug + (n-1)) 4)) trumfu (take n pila)), (mod (jug +(n-1)) 4)) | n <-[0..3] ]
    --hiHaTrampa= [((notElem (pila!!n) (fst (jugadesGen!!n))), (snd (jugadesGen!!n))) |  n <- [0..3]]
    --jugades1 = jugades (ll!!(mod (jug - 1) 4)) trumfu []
    --jugades2 = jugades (ll!!(mod (jug) 4))     trumfu [c1]
    --jugades3 = jugades (ll!!(mod (jug + 1) 4)) trumfu [c1,c2]
    --jugades4 = jugades (ll!!(mod (jug + 2) 4)) trumfu [c1,c2,c3]
    --hiHaTrampa = [((notElem c1 jugades1),(mod (jug - 1) 4)),((notElem c2 jugades2),(mod (jug) 4)), ((notElem c3 jugades3),(mod (jug + 1) 4)),((notElem c4 jugades4),(mod (jug + 2) 4))]

-- Pre :
-- Post :
cartesGuanyades::  Trumfu -> [Carta] -> Int -> ([Carta],[Carta])
cartesGuanyades trumfu [] jugador = ([],[])
cartesGuanyades trumfu (c1:c2:c3:c4:pila) jugador
  | (mod seguentJug 2) /= 0 = (basa ++ (fst res), (snd res))
  | otherwise = ((fst res), basa ++ (snd res))
  where
    basa = [c1,c2,c3,c4]
    guanyador = quiGuanya basa trumfu
    seguentJug = quiSortira jugador (snd guanyador)
    res = (cartesGuanyades trumfu pila seguentJug)

-- Pre :
-- Post :
punts :: [Carta] -> Int
punts llista = sum [ (valor x) | x <- llista] + (div (length llista) 4)

-- Pre :
--Post :
puntsParelles :: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe (Int, Int)
puntsParelles cartesJugadors trumfu partida jug
  | trampa cartesJugadors trumfu partida jug == Nothing = Just (punts (fst resultatPartida), punts (snd resultatPartida))
  | otherwise = Nothing 
  where
    resultatPartida = cartesGuanyades trumfu partida jug

-- Pre :
-- Post :
canviaPosicio :: [Carta] -> Int -> [Carta]
canviaPosicio cartes random = ((filter (/=(cartes!!random)) cartes) ++ [(cartes!!random)])

-- Pre :
-- Post :
barreja :: [Carta] -> [Int] -> [Carta]
barreja cartes random = foldl (canviaPosicio) (cartes) random

-- Pre :
-- Post :
reparteix :: [[Carta]] -> [Carta] -> Int -> [[Carta]]
reparteix mans [] jugador = mans
reparteix [a,b,c,d] (c1:c2:c3:c4:pila) jugador
  | jugador == 1 = reparteix [a ++ [c1,c2,c3,c4], b, c, d] pila (seguentJugador jugador)
  | jugador == 2 = reparteix [a, b ++ [c1,c2,c3,c4], c, d] pila (seguentJugador jugador)
  | jugador == 3 = reparteix [a, b, c ++ [c1,c2,c3,c4], d] pila (seguentJugador jugador)
  | jugador == 4 = reparteix [a, b, c, d ++ [c1,c2,c3,c4]] pila (seguentJugador jugador)
  | otherwise = [a,b,c,d]

-- Pre :
-- Post :
generarPartida :: [[Carta]] -> Trumfu -> Int -> [Carta]
generarPartida [[],[],[],[]] _ _ = []
generarPartida mans trumfu jug = basa ++ generarPartida (extreu mans basa jug) trumfu (properATirar basa trumfu jug)--(quiSortira (quiGuanya ))
  where
    --jug1 = (jug - 1)
    --jug2 = ((seguentJugador jug) - 1)
    --jug3 = ((seguentJugador (seguentJugador jug)) - 1)
    --jug4 = ((seguentJugador (seguentJugador (seguentJugador jug))) - 1)
    --carta = maximum (jugades (mans!!jug1) trumfu [])
    --carta2 = maximum (jugades (mans!!jug2) trumfu [carta])
    --carta3 = maximum (jugades (mans!!jug3) trumfu [carta, carta2])
    --carta4 = maximum (jugades (mans!!jug4) trumfu [carta, carta2, carta3])
    --basa = [carta, carta2, carta3, carta4]
    --basa= [ head cartes1!!n | n <- [0..3]]
    basa= [ maximum (jugades (mans!!(mod (jug + (n-1)) 4)) trumfu (take n basa)) | n <-[0..3] ]



main = do
  seed <- newStdGen
  let random = take 200 (randomRs (0 :: Int, 47) seed)
  let quiReparteix = 2
  let mans = reparteix [[],[],[],[]] (barreja partida1 random) quiReparteix
  let pal = Butifarra
  -- torna el jugador que comença el pal de la partida les mans dels jugadors del 1..4 i la partida
  print $ ((seguentJugador quiReparteix), pal, mans, generarPartida mans (pal) (seguentJugador quiReparteix) )
