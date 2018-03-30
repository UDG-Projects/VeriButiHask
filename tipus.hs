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
trampa :: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe ([Carta],Int, Int)
trampa _ trumfu [] jug = Nothing
trampa ll trumfu (c1:c2:c3:c4:pila) jug =
  if or [fst x | x<-hiHaTrampa] then Just ([c1,c2,c3,c4], (12 - (length (head ll))) + 1 , (head [snd x | x<-hiHaTrampa, (fst x)]) + 1)
  else trampa [(filter (/=c1) (ll!!(mod (jug - 1) 4))),
                (filter (/=c2) (ll!!(mod (jug) 4))),
                (filter (/=c3) (ll!!(mod (jug + 1) 4))),
                (filter (/=c4) (ll!!(mod (jug + 2) 4)))] trumfu pila (snd (quiGuanya [c1,c2,c3,c4] trumfu))
  where
    -- Mirem que les cartes estiguin dintre de jugades
    jugades1 = jugades (ll!!(mod (jug - 1) 4)) trumfu []
    jugades2 = jugades (ll!!(mod (jug) 4))     trumfu [c1]
    jugades3 = jugades (ll!!(mod (jug + 1) 4)) trumfu [c1,c2]
    jugades4 = jugades (ll!!(mod (jug + 2) 4)) trumfu [c1,c2,c3]
    hiHaTrampa = [((notElem c1 jugades1),(mod (jug - 1) 4)),((notElem c2 jugades2),(mod (jug) 4)), ((notElem c3 jugades3),(mod (jug + 1) 4)),((notElem c4 jugades4),(mod (jug + 2) 4))]

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
