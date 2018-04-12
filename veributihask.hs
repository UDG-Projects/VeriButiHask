import System.Random
import Drawable
----------------------------------------------------------------------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------------------------------------------------------------------

testMans = [
  [Carta Cavall Espases, Carta Rei Oros, Carta Quatre Espases, Carta Set Copes, Carta As Espases, Carta Manilla Espases, Carta Manilla Bastos, Carta Sota Oros, Carta Vuit Copes, Carta As Copes, Carta Cinc Oros, Carta Sota Bastos]
  , [Carta Dos Copes, Carta Quatre Copes, Carta Manilla Oros, Carta Cavall Oros, Carta Rei Copes, Carta Cinc Espases, Carta Vuit Espases, Carta Dos Oros, Carta As Oros, Carta Sota Copes, Carta Sis Bastos, Carta Quatre Oros]
  , [Carta Vuit Bastos, Carta Set Espases, Carta Set Oros, Carta Tres Copes, Carta Sota Espases, Carta Tres Bastos, Carta Set Bastos, Carta Vuit Oros, Carta Cinc Copes, Carta As Bastos, Carta Dos Bastos, Carta Rei Bastos]
  , [Carta Sis Oros, Carta Tres Espases, Carta Tres Oros, Carta Manilla Copes, Carta Cavall Copes, Carta Rei Espases, Carta Cavall Bastos, Carta Dos Espases, Carta Sis Espases, Carta Quatre Bastos, Carta Sis Copes, Carta Cinc Bastos]
  ]

test1 = [
 Carta Vuit Bastos, Carta Cavall Bastos, Carta Manilla Bastos, Carta Sis Bastos,
 Carta Sota Bastos, Carta Dos Oros, Carta Dos Bastos, Carta Quatre Bastos,
 Carta Manilla Oros, Carta Set Oros, Carta Tres Oros, Carta Cinc Oros,
 Carta As Oros, Carta Vuit Oros, Carta Sis Oros, Carta Sota Oros,
 Carta Quatre Oros, Carta Tres Copes, Carta Dos Espases, Carta Rei Oros,
 Carta Manilla Espases, Carta Cinc Espases, Carta Set Espases, Carta Tres Espases,
 Carta As Espases, Carta Vuit Espases, Carta Sota Espases, Carta Sis Espases,
 Carta Vuit Copes, Carta Rei Copes, Carta Cinc Copes, Carta Cavall Copes,
 Carta Sota Copes, Carta Tres Bastos, Carta Manilla Copes, Carta Set Copes,
 Carta Rei Espases, Carta Quatre Espases, Carta Dos Copes, Carta Set Bastos,
 Carta Cinc Bastos, Carta Cavall Espases, Carta Quatre Copes, Carta As Bastos,
 Carta Rei Bastos, Carta Sis Copes, Carta As Copes, Carta Cavall Oros]

test2 =
  [
  Carta Sota Copes, Carta Tres Copes, Carta Manilla Copes, Carta Set Copes
  , Carta Sis Copes, Carta As Copes, Carta Dos Copes, Carta Cinc Copes
  , Carta Sota Oros, Carta Manilla Oros, Carta Set Oros, Carta Tres Oros
  , Carta As Oros, Carta Vuit Oros, Carta Sis Oros, Carta Cinc Oros
  , Carta Rei Copes, Carta Set Espases, Carta Cavall Copes, Carta Vuit Copes
  , Carta Quatre Copes, Carta Dos Bastos, Carta Cavall Bastos, Carta Quatre Espases
  , Carta Quatre Oros, Carta Tres Bastos, Carta Quatre Bastos, Carta Rei Oros
  , Carta Manilla Espases, Carta Cinc Espases, Carta Sota Espases, Carta Dos Espases
  , Carta As Espases, Carta Vuit Espases, Carta As Bastos, Carta Tres Espases
  , Carta Manilla Bastos, Carta Sis Bastos, Carta Set Bastos, Carta Cinc Bastos
  , Carta Sota Bastos, Carta Dos Oros, Carta Rei Bastos, Carta Sis Espases
  , Carta Vuit Bastos, Carta Rei Espases, Carta Cavall Espases, Carta Cavall Oros]

capot = [
  Carta Rei Espases, Carta Manilla Espases, Carta Cinc Espases, Carta Sota Espases
  , Carta Manilla Bastos, Carta Sis Bastos, Carta Dos Bastos, Carta Quatre Bastos
  , Carta Sota Bastos, Carta Dos Copes, Carta As Bastos, Carta Cinc Bastos
  , Carta Rei Bastos, Carta Cavall Bastos, Carta Rei Oros, Carta Vuit Espases
  , Carta Set Espases, Carta Dos Espases, Carta As Espases, Carta Manilla Oros
  , Carta Cavall Espases, Carta As Oros, Carta Tres Copes, Carta Sis Espases
  , Carta Quatre Espases, Carta Cavall Oros, Carta Cinc Copes, Carta Tres Espases
  , Carta Sota Oros, Carta Dos Oros, Carta Set Oros, Carta Tres Oros
  , Carta Cinc Oros, Carta Quatre Oros, Carta Vuit Oros, Carta Sis Oros
  , Carta Tres Bastos, Carta Sis Copes, Carta Set Copes, Carta Quatre Copes
  , Carta Set Bastos, Carta Cavall Copes, Carta As Copes, Carta Sota Copes
  , Carta Vuit Bastos, Carta Manilla Copes, Carta Vuit Copes, Carta Rei Copes]

----------------------------------------------------------------------------------------------------------------------------------
-- Tipus
----------------------------------------------------------------------------------------------------------------------------------
data Pal = Oros | Copes | Espases | Bastos deriving (Eq, Enum)
instance Show Pal where
  show Oros = "O"
  show Copes = "C"
  show Espases = "E"
  show Bastos = "B"

data Trumfu = Butifarra | Pal Pal
instance Eq Trumfu where
  Butifarra == Butifarra  = True
  Pal p1 == Pal p2        = p1 == p2
  _ == _                  = False
instance Show Trumfu where
  show (Butifarra) = "Butifarra"
  show (Pal pal)   = show pal

data TipusCarta = Dos | Tres | Quatre | Cinc | Sis | Set | Vuit | Sota | Cavall | Rei | As | Manilla deriving (Eq, Ord, Enum)
instance Show TipusCarta where
  show Sota = "S"
  show Cavall = "C"
  show Rei = "R"
  show As = "A"
  show Manilla = "9"
  show x = show (fromEnum(x) + 2)

data Carta = Carta TipusCarta Pal deriving (Eq)
instance Show Carta where
  show (Carta a b) = show a ++ show b
instance Ord Carta where
  (Carta te pe) <= (Carta td pd) = te <= td
instance Enum Carta where
  toEnum x = (Carta (toEnum (mod x 12)) (toEnum (div x 12)))
  fromEnum (Carta tipus pal) = ((fromEnum tipus)) + ((fromEnum pal) * 12)



----------------------------------------------------------------------------------------------------------------------------------
-- Funcions
----------------------------------------------------------------------------------------------------------------------------------

-- Pre :
-- Post :
baralla :: [Carta]
baralla = [(Carta Dos Oros)..(Carta Manilla Bastos)]

-- Pre :
-- Post :
ronda:: Int -> [Int] -> [Int]
ronda primer llista = if (length llista) < 4 then  (ronda jugador novaLlista) else llista
 where
   jugador = seguentJugador primer
   novaLlista =llista ++ [jugador]


-- Pre :
-- Post :
lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const . drop 1) xs (drop n xs)


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

-- Pre : 0 <= [x && y] < 4
-- Post : [0-3] referent al proper a tirar.
quiSortira :: Int -> Int -> Int
quiSortira x y = (mod ( x + y)  4)

-- Pre : Donat el número d'un jugador
-- Post : Retorna el número del següent jugador
seguentJugador :: Int -> Int
seguentJugador jugador = mod (jugador + 1) 4

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
quiGuanya ll trumfo =  (cartaGuanyadora, (head [index | (index, carta) <- zip [0..] ll, carta == cartaGuanyadora]))
  where
    palGuanyador = palGuanyadorBasa ll trumfo
    cartaGuanyadora = maximum (cartesPal ll palGuanyador)

-- Pre : Rep posició del que guanya actualment i la meva posició.
-- Post : Retorna cert si s'ha de matar fals altrament
saDeMatar :: Int -> Int -> Bool
saDeMatar posGuanya posMeu
  | posMeu - 2 >= 0 = posMeu - 2 /= posGuanya
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
    esticObligatAMatar = saDeMatar (snd guanyador) ((length ll) )
    palBasa = ((\(Carta tc pal)->pal) (head ll))
    cartesJugadorPalBasa = cartesPal cartesJugador palBasa
    cartesJugadorMaten = filter (mata trumfu (fst guanyador)) cartesJugador
    cartesJugadorMatenPalBasa = cartesPal cartesJugadorMaten palBasa

-- Pre : Donades les mans dels jugadors, la basa i qui ha començat a jugar la basa [0 -3]
-- Post : Retorna les mans dels jugadors sense les cartes que s'han jugat a la basa
extreu :: [[Carta]] -> [Carta] -> Int -> [[Carta]]
extreu mans basa jug = [filter (/=cartaJugadorBasa basa jug x) (mans!!x) | x <- [0..3]]

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
  else trampa (extreu ll basa jug) trumfu (drop 4 pila) (properATirar basa trumfu jug) --(quiSortira jug (snd (quiGuanya basa trumfu)))
  where
    -- Mirem que les cartes estiguin dintre de jugades
    basa = take 4 pila
    hiHaTrampa= [((notElem (pila!!n) (jugades (ll!!(mod (jug + n) 4)) trumfu (take n pila))), (mod (jug + n) 4)) | n<-[0..3] ]
    -- es mira per a les 4 cartes de la basa si apareixen a les possibles cartes del jugador (jugades)
    -- es fa una llista amb una tupla (bool, Int).
    --    el boolea representa que la carta ha estat mal tirada (true)
    --    el Int el numero de jugador que l'ha tirat

-- Pre : Donat el trumfu la partida i qui ha començat [0-3] (S'ha d'haver jugat la partida sencera)
-- Post : Retorna les cartes guanyades de cada equip en forma de tupla ([cartes equip 1], [cartes equip 2])
cartesGuanyades::  Trumfu -> [Carta] -> Int -> ([Carta],[Carta])
cartesGuanyades trumfu [] jugador = ([],[])
cartesGuanyades trumfu (c1:c2:c3:c4:pila) jugador
  | (mod seguentJug 2) == 0 = (basa ++ (fst res), (snd res)) -- seran els jugadors 0 i 2
  | otherwise = ((fst res), basa ++ (snd res)) --jugadors 1 i 3
  where
    basa = [c1,c2,c3,c4]
    guanyador = quiGuanya basa trumfu
    seguentJug = quiSortira jugador (snd guanyador)
    res = (cartesGuanyades trumfu pila seguentJug)

-- Pre : Donades una llista de cartes
-- Post : Retorna la suma dels punts de les cartes de la llista sumant 1 punt per basa
punts :: [Carta] -> Int
punts llista = sum [ (valor x) | x <- llista] + (div (length llista) 4)

-- Pre : Donades les mans dels jugadors, el trumfu, la partida i el jugador que ha començat la partida [0-3]
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

-- Pre : Donades les mans dels jugadors (buides al inici), la baralla barrejada i el primer al que es reparteix [0-3]
-- Post : Retorna les mans dels jugadors repartides d'acord amb les normes del joc.
reparteix :: [[Carta]] -> [Carta] -> Int -> [[Carta]]
reparteix mans [] jugador = mans
reparteix mans cartes jugador = reparteix novaMans (drop 4 cartes) (seguentJugador jugador)
 where
  maJugador = (mans!!(jugador))++(take 4 cartes)
  -- construir la ma del seguent jugador
  novaMans = take jugador mans ++ [maJugador] ++ drop (jugador + 1) mans
  -- s'ha de construir una nova llista de novaMans
  -- S'agafen les mans just abans del jugador, les del jugador i la resta de mans fins al Finalitzar

tiraCartaBot :: [Carta] -> Trumfu -> [Carta] -> Carta
tiraCartaBot ma trumfu basa = maximum (jugades ma trumfu basa)

-- Pre : Donades les mans, el trumfu de la partida i qui comença a jugar
-- Post : Genera una partida de butifarra amb el criteri qui s'expressa dins del where ( TODO : Canviar a bassant oriental o occidental)
generarPartida :: [[Carta]] -> Trumfu -> Int -> [Carta]
generarPartida [[],[],[],[]] _ _ = []
generarPartida mans trumfu jug = basa ++ generarPartida (extreu mans basa jug) trumfu (properATirar basa trumfu jug)--(quiSortira (quiGuanya ))
  where
    -- Aquest maximum s'ha de canviar per un escull millor tirada
    basa= [ tiraCartaBot  (mans!!(mod (jug + (n-1)) 4)) trumfu (take n basa) | n <-[0..3] ]


-- Pre : Donada la partida jugada fins el moment i una carta qualsevol
-- Post : Retorna cert si aquesta carta és ferma, fals altrament. (només mira el pal)
esFerma :: [Carta] -> Carta -> Bool
esFerma partida (Carta Manilla pal) = True
esFerma partida (Carta tipus pal) = and [ elem x partida | x <- [(cartaSeguentMajor (Carta tipus pal))..(Carta Manilla pal)]]

----------------------------------------------------------------------------------------------------------------------------------
-- PRESUMPTA IA
----------------------------------------------------------------------------------------------------------------------------------

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






----------------------------------------------------------------------------------------------------------------------------------
-- MONADES MENUS
----------------------------------------------------------------------------------------------------------------------------------
mostraMenu = do
  putStrLn("0 - Finalitzar Programa")
  putStrLn("1 - Remenar Baralla")
  putStrLn("2 - Repartir")
  putStrLn("3 - Testos")
  putStrLn("4 - Jugar")

mostraMenuTrampa = do
  putStrLn("0 - Sortir del menu Trampa")
  putStrLn("1 - No hi ha error test1")
  putStrLn("2 - No hi ha error test2 pal Butifarra")
  putStrLn("3 - No hi ha error capot")
  putStrLn("4 - Error Falla de gallines")
  putStrLn("5 - Error Refalla de gallines") -- Mata Amb trunfu quan encara l'in queden del pal de la basa
  putStrLn("6 - Error S'escapen ") --No dona l'As quan l'ha de posar
  putStrLn("7 - Error No Mata") -- el jugador no mata quan li toca matar

-- Pre :
-- Post :
tiraCarta :: [Carta] -> Trumfu -> [Carta] -> Bool -> IO (Carta)
tiraCarta ma trumfu basa esJugadorReal = do
  if esJugadorReal then do
    putStrLn("Basa actual:")
    putStrLn( show basa)
    putStrLn( "Les teves cartes:")
    putStrLn( show (ma) )
    putStrLn("Tira una carta")
    opcio <- getLine
    let numOp = (read opcio :: Int)
  --  partida++[numOP]
    let carta = (ma!!numOp)
    return (carta)
  else do
    return (tiraCartaBot ma trumfu basa)

-- Pre :
-- Post :
jugar :: [[Carta]] -> Trumfu -> [Int] -> [Carta] -> Int -> IO ([Carta])
jugar [[],[],[],[]] _ _  partida _ = do return (partida)
jugar mans trumfu llistaJugadors partida playerReal = do

  let quiTira = (llistaJugadors!!(mod (length partida) 4))
  let quiTirara = (seguentJugador quiTira)
  let basa = lastN (mod (length partida) 4) partida
  putStrLn( show(quiTira==playerReal))
  carta <- (tiraCarta (mans!!quiTira) trumfu basa (quiTira==playerReal))
  let novaBasa = basa++[carta]
  putStrLn("El jugador " ++ (show quiTira) ++ " tira " ++ (show carta))

  if (llistaJugadors!!3) == quiTira then do
    let guanyador = properATirar novaBasa trumfu (head llistaJugadors)
    putStrLn( "La basa final es" ++ (show novaBasa) ++ " i el que ha guanyat es " ++ (show guanyador))
    jugar (extreu mans (novaBasa) (head llistaJugadors)) trumfu (ronda guanyador [guanyador]) (partida++[carta]) playerReal
  else do
    jugar mans trumfu llistaJugadors (partida++[carta]) playerReal

-- Donat el test (trampa o punts parelles), Les cartes del jugadors, el trumfu, la partida que s'ha jugat i el número de jugador que l'ha començat
-- Pinta per pantalla en un format llegible el resultat d'executar el test.
doTest textTitol numExplicacio mans trumfu partida jugador = do
  putStrLn((titol textTitol))
  putStrLn("## MANS : ")
  putStrLn(separador)
  putStrLn("## Ma del jugador 1 -> " ++ show (mans!!0))
  putStrLn("## Ma del jugador 2 -> " ++ show (mans!!1))
  putStrLn("## Ma del jugador 3 -> " ++ show (mans!!2))
  putStrLn("## Ma del jugador 4 -> " ++ show (mans!!3))
  putStrLn(separador)
  putStrLn("## PARTIDA : ")
  putStrLn(separador)
  putStrLn(show partida)
  putStrLn(separador)
  putStrLn((explicacioTest 1))
  putStrLn(separador)
  putStrLn((capcalera jugador (seguentJugador jugador) (show trumfu)))
  putStrLn("## Hi ha Trampa = " ++ show (trampa mans trumfu partida (seguentJugador jugador)))

----------------------------------------------------------------------------------------------------------------------------------
-- Programa Principal
----------------------------------------------------------------------------------------------------------------------------------


menuTrampes = do
  opcio <- getLine
  let numOpcio = read opcio
  if numOpcio == 0 then do
    putStrLn("Retrocedir")
  else if numOpcio == 1 then do
    doTest "Test 1" 1 testMans (Pal Oros) test1 2
    menuTrampes
  else if numOpcio == 2 then do
    doTest "Test 2" 2 testMans Butifarra test2 1
    --putStrLn(show (trampa testMans Butifarra test2 2))
    menuTrampes
  else if numOpcio == 3 then do
    doTest "Test 3" 3 testMans (Pal Bastos) capot 3
    --putStrLn(show (trampa testMans (Pal Bastos) capot 4))
    menuTrampes
  else if numOpcio == 4 then do
    putStrLn("Es 1")
    menuTrampes
  else if numOpcio == 5 then do
    putStrLn("Es 1")
    menuTrampes
  else if numOpcio == 6 then do
    putStrLn("Es 1")
    menuTrampes
  else if numOpcio == 7 then do
    menuTrampes
  else do
    putStrLn("L'has cagat. Tria bé coi!!")
    menuTrampes


programa barallaCartes ra = do
  --Generar Baralla
  mostraMenu
  opcio <- getLine
  let numOpcio = read opcio
  if numOpcio == 0 then
    putStrLn("Numero 0")
  else if numOpcio == 1 then do
    --Remenar la baralla
    let barrejades = (barreja barallaCartes ra)
    putStrLn(show barrejades)
    programa barrejades ra
  else if numOpcio == 2 then do
    -- Repartir cartes
    jug <- getLine
    let numJug = read jug
    let mans = reparteix [[],[],[],[]] (barreja barallaCartes ra) numJug
    putStrLn(show mans)
    programa barallaCartes ra
  else if numOpcio == 3 then do
    --Trampa
    mostraMenuTrampa
    menuTrampes
    programa barallaCartes ra
  else if numOpcio == 4 then do

    main
  else do
    putStrLn("Opcio incorrecte")
    main


main = do
  seed <- newStdGen
  let random = take 200 (randomRs (0 :: Int, 47) seed)
  programa baralla random
--  let quiReparteix = 2
--  let mans = reparteix [[],[],[],[]] (barreja baralla random) quiReparteix
--  partidaJugada <- jugar mans (Pal Oros) (ronda (seguentJugador quiReparteix) [(seguentJugador quiReparteix)]) [] 2
--  putStrLn("hola")