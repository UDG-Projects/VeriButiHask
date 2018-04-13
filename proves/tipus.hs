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
data Pal = Oros | Copes | Espases | Bastos deriving (Show, Eq, Enum, Read)


data Trumfu = Butifarra | Pal Pal
instance Eq Trumfu where
  Butifarra == Butifarra  = True
  Pal p1 == Pal p2        = p1 == p2
  _ == _                  = False
instance Show Trumfu where
  show (Butifarra) = "Butifarra"
  show (Pal pal)   = show pal


data TipusCarta = Dos | Tres | Quatre | Cinc | Sis | Set | Vuit | Sota | Cavall | Rei | As | Manilla deriving (Show, Eq, Ord, Enum, Read)


data Carta = Carta TipusCarta Pal deriving (Read)
instance Eq Carta where
  Carta te pe == Carta td pd = (te == td) && (pe == pd)
instance Show Carta where
  show (Carta a Bastos) = show a ++ " de " ++ show Bastos
  show (Carta a Copes) = show a ++ " de " ++ show Copes
  show (Carta a b)   = show a ++ " d'" ++ show b
instance Ord Carta where
  (Carta te pe) <= (Carta td pd) = te <= td
instance Enum Carta where
  toEnum x = (Carta (toEnum (mod x 12)) (toEnum (div x 12)))
  fromEnum (Carta tipus pal) = ((fromEnum tipus)) + ((fromEnum pal) * 12)


-- Funcions -------------------------------------------------------------------------------

-- Pre : Donada una carta
-- Post : Retorna el seu valor en punts
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

-- Pre : Donat el número d'un jugador
-- Post : Retorna el número del següent jugador
seguentJugador :: Int -> Int
seguentJugador jugador
  | jugador == 4 = 1
  | otherwise = jugador + 1

-- Pre : Donada una basa, el jugador que l'ha començat i el jugador que volem saber que ha tirat
-- Post : Retorna la carta jugada per jugador sent comenca qui ha començat la basa
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

-- Pre : Donada una carta
-- Post : Retorna si és possible que hi hagi una carta superior del mateix pal
teCartaMajorDelPal :: Carta -> Bool
teCartaMajorDelPal (Carta x pal)
  | x == Manilla = False
  | otherwise = True

-- Pre : Donada una carta (Havent validat si té major amb teCartaMajorDelPal)
-- Post : Retorna la carta següent en l'escala de valors
cartaSeguentMajor :: Carta -> Carta
cartaSeguentMajor (Carta x p)
  | x == As       = (Carta Manilla p)
  | x == Dos      = (Carta Tres p)
  | x == Tres     = (Carta Quatre p)
  | x == Quatre   = (Carta Cinc p)
  | x == Cinc     = (Carta Sis p)
  | x == Sis      = (Carta Set p)
  | x == Set      = (Carta Vuit p)
  | x == Vuit     = (Carta Sota p)
  | x == Sota     = (Carta Cavall p)
  | x == Cavall   = (Carta Rei p)

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

-- Pre : Donada una condició i dues llistes
-- Post : Escull l1 si b i l2 si no b
selecciona :: Bool -> [Carta] -> [Carta] -> [Carta]
selecciona b l1 l2 = if b then l1 else l2

-- Pre :
-- Post :
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

-- Pre : Donades les mans dels jugadors, la basa i qui ha començat a jugar la basa
-- Post : Retorna les mans dels jugadors sense les cartes que s'han jugat a la basa
extreu :: [[Carta]] -> [Carta] -> Int -> [[Carta]]
extreu mans basa jug = [filter (/=cartaJugadorBasa basa jug x) (mans!!(x - 1)) | x <- [1..4]]
-- extreu mans basa jug = [(filter (/=(cartaJugadorBasa basa jug 1)) (mans!!0)),
                        --(filter (/=(cartaJugadorBasa basa jug 2)) (mans!!1)),
                        --(filter (/=(cartaJugadorBasa basa jug 3)) (mans!!2)),
                        --(filter (/=(cartaJugadorBasa basa jug 4)) (mans!!3))]

-- Pre : Donada una basa, el trumfu i el primer que ha jugat
-- Post : Retorna el jugador que començarà la següent basa
properATirar :: [Carta] -> Trumfu -> Int -> Int
properATirar basa trumfu jug = (quiSortira jug (snd (quiGuanya basa trumfu)))

-- Pre : Donades les mans dels jugadors, el trumfu de la partida, la partida sencera i qui ha començat jugant
-- Post : Retorna Nothing si no hi ha hagut trmapa o Just (basa on hi ha la trampa, numero de basa, jugador que ha tirat la carta)
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

-- Pre : Donat el trumfu la partida i qui ha començat (S'ha d'haver jugat la partida sencera)
-- Post : Retorna les cartes guanyades de cada equip en forma de tupla ([cartes equip 1], [cartes equip 2])
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

-- Pre : Donades una llista de cartes
-- Post : Retorna la suma dels punts de les cartes de la llista sumant 1 punt per basa
punts :: [Carta] -> Int
punts llista = sum [ (valor x) | x <- llista] + (div (length llista) 4)

-- Pre : Donades les mans dels jugadors, el trumfu, la partida i el jugador que ha començat la partida
-- Post : Retorna nothing si s'ha fet trampa, o (Punts equip 1, Punts equip 2) en cas que no s'hagi fet trampa
puntsParelles :: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe (Int, Int)
puntsParelles cartesJugadors trumfu partida jug
  | trampa cartesJugadors trumfu partida jug == Nothing = Just (punts (fst resultatPartida), punts (snd resultatPartida))
  | otherwise = Nothing 
  where
    resultatPartida = cartesGuanyades trumfu partida jug

-- Pre : Donada una llista de cartes i un nombre aleatori
-- Post : mou la carta que està a la posició random a la cua de la llista de cartes
canviaPosicio :: [Carta] -> Int -> [Carta]
canviaPosicio cartes random = ((filter (/=(cartes!!random)) cartes) ++ [(cartes!!random)])

-- Pre : Donades la baralla de cartes i un llistat de randoms infinit
-- Post : Retorna la baralla de caretes barrejada
barreja :: [Carta] -> [Int] -> [Carta]
barreja cartes random = foldl (canviaPosicio) (cartes) random

-- Pre : Donades les mans dels jugadors (buides al inici), la baralla barrejada i el primer al que es reparteix
-- Post : Retorna les mans dels jugadors repartides d'acord amb les normes del joc.
reparteix :: [[Carta]] -> [Carta] -> Int -> [[Carta]]
reparteix mans [] jugador = mans
reparteix [a,b,c,d] (c1:c2:c3:c4:pila) jugador
  | jugador == 1 = reparteix [a ++ [c1,c2,c3,c4], b, c, d] pila (seguentJugador jugador)
  | jugador == 2 = reparteix [a, b ++ [c1,c2,c3,c4], c, d] pila (seguentJugador jugador)
  | jugador == 3 = reparteix [a, b, c ++ [c1,c2,c3,c4], d] pila (seguentJugador jugador)
  | jugador == 4 = reparteix [a, b, c, d ++ [c1,c2,c3,c4]] pila (seguentJugador jugador)
  | otherwise = [a,b,c,d]

-- Pre : Donades les mans, el trumfu de la partida i qui comença a jugar
-- Post : Genera una partida de butifarra amb el criteri qui s'expressa dins del where ( TODO : Canviar a bassant oriental o occidental)
generarPartida :: [[Carta]] -> Trumfu -> Int -> [Carta]
generarPartida [[],[],[],[]] _ _ = []
generarPartida mans trumfu jug = basa ++ generarPartida (extreu mans basa jug) trumfu (properATirar basa trumfu jug)--(quiSortira (quiGuanya ))
  where
    -- Aquest maximum s'ha de canviar per un escull millor tirada
    basa= [ maximum (jugades (mans!!(mod (jug + (n-1)) 4)) trumfu (take n basa)) | n <-[0..3] ]
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


-- Pre : Donada la partida jugada fins el moment i una carta qualsevol
-- Post : Retorna cert si aquesta carta és ferma, fals altrament. (només mira el pal)
esFerma :: [Carta] -> Carta -> Bool
esFerma partida (Carta Manilla pal) = True
esFerma partida (Carta tipus pal) = and [ elem x partida | x <- [(cartaSeguentMajor (Carta tipus pal))..(Carta Manilla pal)]]


-- Pre : Donada la partida fins el moment i les cartes d'un jugador, el trumfu de la partida i si el trumfu l'ha fet el company
-- Post : Retorna la carta més adient per realitzar una sortida.
escullMillorSortida :: [Carta] -> [Carta] -> Trumfu -> Carta
escullMillorSortida partida ma trumfu
  -- Sortida de Manilla As. (Quan tens manilla i as d'un mateix pal, amb el trumfu els has hagut de fer tu o el company)
  | (elem (Carta Manilla Oros) ma) && (elem (Carta As Oros) ma) = (Carta Manilla Oros)
  | (elem (Carta Manilla Copes) ma) && (elem (Carta As Copes) ma) = (Carta Manilla Copes)
  | (elem (Carta Manilla Espases) ma) && (elem (Carta As Espases) ma) = (Carta Manilla Espases)
  | (elem (Carta Manilla Bastos) ma) && (elem (Carta As Bastos) ma) = (Carta Manilla Bastos)
  -- | and [(elem (Carta Manilla pal) ma) && (elem (Carta As pal) ma) | pal <- [Oros .. Bastos] ] = (Carta Manilla pal)
  -- Sortida de Manilla Rei. (Quan tens manilla i rei d'un mateix pal que no és trumfu)
  | (elem (Carta Manilla Oros) ma) && (elem (Carta Rei Oros) ma) && trumfu /= (Pal Oros) = (Carta Manilla Oros)
  | (elem (Carta Manilla Copes) ma) && (elem (Carta Rei Copes) ma) && trumfu /= (Pal Copes) = (Carta Manilla Copes)
  | (elem (Carta Manilla Espases) ma) && (elem (Carta Rei Espases) ma) && trumfu /= (Pal Espases) = (Carta Manilla Espases)
  | (elem (Carta Manilla Bastos) ma) && (elem (Carta Rei Bastos) ma) && trumfu /= (Pal Bastos) = (Carta Manilla Bastos)
  -- Sortida protegir As. (Quan tens As i una carta entre el 7 i el rei d'un mateix pal, llavors jugues la carta inferior per protegir el teu as) Aquesta jugada requereix que tinguis més de tres cartes del pal que tens la coincidència.

  -- Sortir de semifallo (Només al inici de la partida o amb poques mans jugades)

  -- Sortida petita Butifarra. (Quan tens una manilla i una carta petita d'un pal (es juga la petita per marcar al contrari que tens una manilla))
  | otherwise = maximum fermes
  where
    fermes = filter (esFerma partida) ma
    oros = [x | x <-ma, (\(Carta tp p)->p==Oros) x]
    bastos = [x | x <-ma, (\(Carta tp p)->p==Bastos) x]
    espases = [x | x <-ma, (\(Carta tp p)->p==Espases) x]
    copes = [x | x <-ma, (\(Carta tp p)->p==Copes) x]

-- Pre : Donada la partida, la ma del que li toca tirar, la basa del moment, el trumfu i el jugador que ha començat a tirar
-- Post : Retorna la carta més adient per seguir jugant.
escullCartaATirar :: [Carta] -> [Carta] -> [Carta] -> Trumfu -> Int -> Carta
-- Estic sortint i per tant miro de que sortir.
escullCartaATirar partida ma [] trumfu primerJugador = escullMillorSortida partida ma trumfu
--escullCartaATirar partida ma [c] trumfu primerJugador =
--escullCartaATirar partida ma [c1,c2] trumfu primerJugador =
--escullCartaATirar partida ma [c1,c2,c3] trumfu primerJugador =

main = do
  seed <- newStdGen
  let random = take 200 (randomRs (0 :: Int, 47) seed)
  let quiReparteix = 2
  let mans = reparteix [[],[],[],[]] (barreja partida1 random) quiReparteix
  let pal = Butifarra
  -- torna el jugador que comença el pal de la partida les mans dels jugadors del 1..4 i la partida
  print $ ((seguentJugador quiReparteix), pal, mans, generarPartida mans (pal) (seguentJugador quiReparteix) )
