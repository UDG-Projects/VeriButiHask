import System.Random
import Drawable
----------------------------------------------------------------------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------------------------------------------------------------------

testMans = [
  [Carta Cavall Espases, Carta Rei Oros, Carta Quatre Espases, Carta Set Copes, Carta As Espases, Carta Manilla Espases, Carta Manilla Bastos, Carta Sota Oros, Carta Vuit Copes, Carta As Copes, Carta Cinc Oros, Carta Sota Bastos]
  , [Carta Dos Copes, Carta Quatre Copes, Carta Manilla Oros, Carta Cavall Oros, Carta Rei Copes, Carta Cinc Espases, Carta Vuit Espases, Carta Dos Oros, Carta As Oros, Carta Sota Copes, Carta Sis Bastos, Carta Quatre Oros]
  , [Carta Vuit Bastos, Carta Set Espases, Carta Set Oros, Carta Tres Copes, Carta Sota Espases, Carta Tres Bastos, Carta Set Bastos, Carta Vuit Oros, Carta Cinc Copes, Carta As Bastos, Carta Dos Bastos, Carta Rei Bastos]
  , [Carta Sis Oros, Carta Tres Espases, Carta Tres Oros, Carta Manilla Copes, Carta Cavall Copes, Carta Rei Espases, Carta Cavall Bastos, Carta Dos Espases, Carta Sis Espases, Carta Quatre Bastos, Carta Sis Copes, Carta Cinc Bastos]]

maSescapa = [
  [Carta Cavall Espases, Carta Rei Oros, Carta Quatre Espases, Carta Set Copes, Carta As Espases, Carta Manilla Espases, Carta Manilla Bastos, Carta Sota Oros, Carta Vuit Copes, Carta As Copes, Carta Cinc Oros, Carta Sota Bastos]
  ,[Carta Dos Bastos, Carta Quatre Copes, Carta Manilla Oros, Carta Cavall Oros, Carta Manilla Copes, Carta Cinc Espases, Carta Vuit Espases, Carta Dos Oros, Carta As Oros, Carta Sota Copes, Carta Sis Bastos, Carta Quatre Oros]
  ,[Carta Vuit Bastos, Carta Set Espases, Carta Set Oros, Carta Tres Copes, Carta Sota Espases, Carta Tres Bastos, Carta Set Bastos, Carta Vuit Oros, Carta Cinc Copes, Carta As Bastos, Carta Dos Copes, Carta Rei Bastos]
  ,[Carta Sis Oros, Carta Tres Espases, Carta Tres Oros, Carta Rei Copes, Carta Cavall Copes, Carta Rei Espases, Carta Cavall Bastos, Carta Dos Espases, Carta Sis Espases, Carta Quatre Bastos, Carta Sis Copes, Carta Cinc Bastos]]

mansJugadaMaquina = [
  [Carta Cavall Bastos, Carta Cavall Copes, Carta Manilla Oros, Carta Cinc Espases, Carta As Espases, Carta Dos Oros, Carta Dos Bastos, Carta Dos Espases, Carta Sis Oros, Carta Set Bastos, Carta Sis Copes, Carta Set Oros],
  [Carta As Bastos, Carta Vuit Copes, Carta Vuit Oros, Carta Manilla Espases, Carta Rei Espases, Carta Cinc Oros, Carta Sota Bastos, Carta Sota Espases, Carta Quatre Oros, Carta Cinc Bastos, Carta Dos Copes, Carta Tres Oros],
  [Carta Manilla Bastos, Carta Manilla Copes, Carta As Oros, Carta Set Espases, Carta Sis Espases, Carta Rei Oros, Carta Quatre Bastos, Carta Rei Copes, Carta Sota Oros, Carta Tres Bastos, Carta Sota Copes, Carta Set Copes],
  [Carta Rei Bastos, Carta As Copes, Carta Cavall Oros, Carta Cavall Espases, Carta Vuit Espases, Carta Cinc Copes, Carta Vuit Bastos, Carta Quatre Espases, Carta Quatre Copes, Carta Sis Bastos, Carta Tres Copes, Carta Tres Espases]]

partidaJugadaMaquina = [ Carta Manilla Bastos, Carta Rei Bastos, Carta Cavall Bastos ,Carta As Bastos
  , Carta Manilla Copes , Carta As Copes ,Carta Cavall Copes, Carta Vuit Copes
  , Carta As Oros, Carta Cavall Oros ,Carta Manilla Oros, Carta Vuit Oros
  , Carta Cinc Espases, Carta Manilla Espases, Carta Set Espases ,Carta Cavall Espases
  , Carta Rei Espases, Carta Sis Espases, Carta Vuit Espases, Carta As Espases
  , Carta Dos Oros, Carta Cinc Oros, Carta Rei Oros, Carta Cinc Copes
  , Carta Vuit Bastos, Carta Dos Bastos, Carta Sota Bastos, Carta Quatre Bastos
  , Carta Sota Espases, Carta Rei Copes ,Carta Quatre Espases,Carta Dos Espases
  , Carta Sota Oros,Carta Quatre Copes,Carta Sis Oros,Carta Quatre Oros
  , Carta Sis Bastos,Carta Set Bastos,Carta Cinc Bastos,Carta Tres Bastos
  , Carta Sis Copes,Carta Dos Copes,Carta Sota Copes ,Carta Tres Copes
  , Carta Set Copes, Carta Tres Espases,Carta Set Oros,Carta Tres Oros]

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

fallaGallina = [
 Carta Vuit Bastos, Carta Cavall Bastos, Carta Manilla Bastos, Carta Dos Oros,
 Carta Sota Bastos, Carta Sis Bastos, Carta Dos Bastos, Carta Quatre Bastos,
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

refallaGallina = [
 Carta Vuit Bastos, Carta Cavall Bastos, Carta Manilla Bastos, Carta Sis Bastos,
 Carta Sota Bastos, Carta Dos Oros, Carta Vuit Oros, Carta Quatre Bastos,
 Carta Manilla Oros, Carta Set Oros, Carta Tres Oros, Carta Cinc Oros,
 Carta As Oros, Carta Dos Bastos, Carta Sis Oros, Carta Sota Oros,
 Carta Quatre Oros, Carta Tres Copes, Carta Dos Espases, Carta Rei Oros,
 Carta Manilla Espases, Carta Cinc Espases, Carta Set Espases, Carta Tres Espases,
 Carta As Espases, Carta Vuit Espases, Carta Sota Espases, Carta Sis Espases,
 Carta Vuit Copes, Carta Rei Copes, Carta Cinc Copes, Carta Cavall Copes,
 Carta Sota Copes, Carta Tres Bastos, Carta Manilla Copes, Carta Set Copes,
 Carta Rei Espases, Carta Quatre Espases, Carta Dos Copes, Carta Set Bastos,
 Carta Cinc Bastos, Carta Cavall Espases, Carta Quatre Copes, Carta As Bastos,
 Carta Rei Bastos, Carta Sis Copes, Carta As Copes, Carta Cavall Oros]

sescapa = [
  Carta Vuit Bastos, Carta Cavall Bastos, Carta Manilla Bastos, Carta Dos Bastos
  , Carta Sota Bastos, Carta Sis Bastos, Carta As Bastos, Carta Quatre Bastos
  , Carta Cinc Copes, Carta Cavall Copes, Carta Set Copes, Carta Manilla Copes
  , Carta Manilla Oros, Carta Set Oros, Carta Tres Oros, Carta Cinc Oros
  , Carta As Oros, Carta Vuit Oros, Carta Sis Oros, Carta Sota Oros
  , Carta Sota Copes, Carta Dos Copes, Carta Rei Copes, Carta As Copes
  , Carta Manilla Espases, Carta Cinc Espases, Carta Set Espases, Carta Dos Espases
  , Carta As Espases, Carta Vuit Espases, Carta Sota Espases, Carta Tres Espases
  , Carta Cavall Espases, Carta Dos Oros, Carta Tres Espases, Carta Rei Espases
  , Carta Quatre Oros, Carta Tres Bastos, Carta Cinc Bastos, Carta Rei Oros
  , Carta Quatre Espases, Carta Cavall Oros, Carta Set Bastos, Carta Sis Espases
  , Carta Quatre Copes, Carta Rei Bastos, Carta Sis Copes, Carta Vuit Copes]

----------------------------------------------------------------------------------------------------------------------------------
-- Tipus
----------------------------------------------------------------------------------------------------------------------------------
data Pal = Oros | Copes | Espases | Bastos deriving (Eq, Enum)
instance Show Pal where
  show Oros = "O"
  show Copes = "C"
  show Espases = "E"
  show Bastos = "B"

data Trumfu = Butifarra | Pal Pal deriving (Eq)
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

-- Pre : True
-- Post : Retorna una baralla de cartes de Butifarra ordenada del Dos d'oros a la Manilla De Bastos
baralla :: [Carta]
baralla = [(Carta Dos Oros)..(Carta Manilla Bastos)]

-- Pre : TODO: [ (seguentJugador (y+x))  | x <-[0..2]]
-- Post :
ronda:: Int -> [Int]
ronda primer = primer:[ (seguentJugador (primer+x)) | x <-[0..2]]
--ronda primer llista = if (length llista) < 4 then  (ronda jugador novaLlista) else llista
-- where
--   jugador = seguentJugador primer
--   novaLlista =llista ++ [jugador]


-- Pre : Donat un nombre n i una llista d'elements
-- Post : Retorna els n ultims elements de la llista
lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const . drop 1) xs (drop n xs)


-- Pre : Donada una carta
-- Post : Retorna el seu valor en punts en seguint la puntuacio a la Butifarra
valor :: Carta -> Int
valor (Carta tc _)
  | tc == Manilla = 5
  | tc == As      = 4
  | tc == Rei     = 3
  | tc == Cavall  = 2
  | tc == Sota    = 1
  | otherwise     = 0

-- Pre : 0 <= [x && y] < 4 Donat el jugador actual (x) i la posisico del que ha guanyat la basa (y)
-- Post : retorna el num de jugador que ha de començar la seguent basa [0-3].
quiSortira :: Int -> Int -> Int
quiSortira x y = (mod ( x + y)  4)

-- Pre : Donat el número d'un jugador
-- Post : Retorna el número del següent jugador
seguentJugador :: Int -> Int
seguentJugador jugador = mod (jugador + 1) 4

-- Pre : Donada una basa, el jugador que l'ha començat i el jugador buscat
-- Post : Retorna la carta que ha jugat el jugador que busquem
cartaJugadorBasa :: [Carta] -> Int -> Int -> Carta
cartaJugadorBasa (carta:pila) comenca jugador
  | comenca == jugador = carta
  | otherwise = cartaJugadorBasa pila (seguentJugador comenca) jugador

-- Pre : Donat el tumfu de la partida i dues cartes Mira si la primera carta mata a la segona
-- Post : Retorna si la primera carta mata a la segona tinguent en compte el trumfu de la partida
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

-- Pre : Donada una llista de cartes i un pal
-- Post : Retorna les cartes de la llista que siguin del pal demanat
cartesPal :: [Carta] -> Pal -> [Carta]
cartesPal ll palDemanat = filter (\(Carta t p)->p == palDemanat) ll

-- Pre : Llista != [] Donada una basa i el trumfu de la partida
-- Post : Retorna el pal guanyador de la basa tinguent en compte el tumfu
palGuanyadorBasa :: [Carta] -> Trumfu -> Pal
palGuanyadorBasa [] trumfo = error "No em pots passar una llista buida animal! "
palGuanyadorBasa ll Butifarra = head [ pal | (Carta t pal) <-ll ]
palGuanyadorBasa ll (Pal trumfu) = if length (cartesPal ll trumfu) > 0 then trumfu else head [ pal | (Carta t pal) <-ll ]

-- Pre : Llista != [] Donada una basa i el trumfo de la partida
-- Post : retorna una tupla amb la carta guanyadora i la posicio de la carta guanyadora a la basa
quiGuanya :: [Carta] -> Trumfu -> (Carta, Int)
quiGuanya [] trumfo = error "No em pots passar una llista buida Animal! "
quiGuanya ll trumfo =  (cartaGuanyadora, (head [index | (index, carta) <- zip [0..] ll, carta == cartaGuanyadora]))
  where
    palGuanyador = palGuanyadorBasa ll trumfo
    cartaGuanyadora = maximum (cartesPal ll palGuanyador)

-- Pre : Donada la posicio del jugador que guanya [0-3] i la posicio del jugador actual [0-3].
-- Post : Retorna cert si el jugador actual ha de matar fals altrament (guanya el company del jugador actual)
saDeMatar :: Int -> Int -> Bool
saDeMatar posGuanya posMeu
  | posMeu - 2 >= 0 = posMeu - 2 /= posGuanya
  | otherwise = True

-- Pre : Donada una condició i dues llistes
-- Post : Escull l1 si b i l2 si no b
selecciona :: Bool -> [a] -> [a] -> [a]
selecciona b l1 l2 = if b then l1 else l2

-- Pre : Donada la ma de jugador, el trumfu de la partida i la basa actual
-- Post : Retorna les cartes que pot tirar el Jugador en funcio de la basa, el trumfu i les ma que tingui segons les normes de la Butifarra
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
    Just (basa, (12 - (length (head ll))) + 1 , (head [snd x | x<-hiHaTrampa, (fst x)]))
  else trampa (extreu ll basa jug) trumfu (drop 4 pila) (properATirar basa trumfu jug) --(quiSortira jug (snd (quiGuanya basa trumfu)))
  where
    -- Mirem que les cartes estiguin dintre de jugades
    basa = take 4 pila
    hiHaTrampa= [((notElem (pila!!n) (jugades (ll!!(mod (jug + n) 4)) trumfu (take n pila))), (mod (jug + n) 4)) | n<-[0..3] ]
    -- es mira per a les 4 cartes de la basa si apareixen a les possibles cartes del jugador (jugades)
    -- es fa una llista amb una tupla (bool, Int).
    --    el boolea representa que la carta ha estat mal tirada (true)
    --    el Int el numero de jugador que l'ha tirat

-- Pre : Donat el trumfu la partida i el jugador que ha començat [0-3] (S'ha d'haver jugat la partida sencera)
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
  novaMans = (take jugador mans) ++ [maJugador] ++ (drop (jugador + 1) mans)
  -- s'ha de construir una nova llista de novaMans
  -- S'agafen les mans just abans del jugador, les del jugador i la resta de mans fins al Finalitzar



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

-- Pre : Donada la ma del jugador
-- Post : Retorna cert si dins la ma hi ha les cartes adients com per contrar
pucContrar :: [Carta] -> Bool
pucContrar ma = (length manilles) > 1
  where
    manilles = [ x | x <- ma , (\(Carta tp p) -> tp == Manilla) x ]

-- Pre : Donada la ma del jugador
-- Post : Retorna cert si dins la ma hi ha les cartes adients per cantar Butifarra
tincButifarra :: [Carta] -> Bool
tincButifarra ma = (length asos) >= 1 && (length manilles) >= 2
  where
    asos = [ x | x <- ma , (\(Carta tp p) -> tp == As) x ]
    manilles = [ x | x <- ma , (\(Carta tp p) -> tp == Manilla) x ]

-- Pre : Doanda la ma del jugador
-- Post : Retorna cert si dins la ma hi ha fallo o semifallo (0 o 1 sola carta d'un pal concret)
tincSemiFalloOFallo :: [Carta] -> Bool
tincSemiFalloOFallo ma = or [ (length y) <= 1 | y <- [ cartesPal ma x | x <- [(Oros)..(Bastos)]]]

-- Pre : Donadts els punts actuals dels equips en forma de tupla (puntsE1, puntsE2), els punts que han fet cada equip en forma de tupla (pE1, pE2) i el multiplicador e la partida
-- Post : Retorna la nova puntuacio en forma de tupla (pE1, pE2) dels equips seguint els criteris del joc
sumaResultat :: (Int,Int) -> (Int, Int) -> Int -> (Int, Int)
sumaResultat (actualEq1, actualEq2) (resEq1, resEq2) multiplicador =
  (actualEq1 + ((*) (fst(diferencia)) multiplicador), actualEq2 + ((*) (snd(diferencia)) multiplicador))
  where
    diferencia = ((if (36 - resEq1) < 0 then 0 else (36 - resEq1)), (if (36 - resEq2) < 0 then 0 else (36 - resEq2)))

-- Pre : Donada una ma i si està o no obligat a fer trumfu
-- Post : retorna nothing si pot no fer-ho i no ho fa o El trumfu escollit.
escullTrumfu :: [Carta] -> Bool -> Maybe (Trumfu)
escullTrumfu ma obligat
  | tincButifarra ma = Just (Butifarra)
  | obligat || (tincSemiFalloOFallo ma) = Just maxPal
  | otherwise = Nothing
  where
    maxPal = (\(Carta tp p)->(Pal p)) (head (snd (maximum [ (length y, y) | y <- [cartesPal ma x | x <- [(Oros)..(Bastos)] ] ] ) ) )

----------------------------------------------------------------------------------------------------------------------------------
-- PRESUMPTA IA
----------------------------------------------------------------------------------------------------------------------------------

-- Pre : Donada la ma del Bot, el trumfu de la partida i la basa actual
-- Post : retorna la carta que tirarà el Bot segons l'estat de la basa actual
tiraCartaBot :: [Carta] -> Trumfu -> [Carta] -> Carta
tiraCartaBot ma trumfu basa = maximum (jugades ma trumfu basa)

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
-- MONADES TESTING
----------------------------------------------------------------------------------------------------------------------------------
-- Pre: Donada les mans dels jugadors
-- Post: Pinta per pantalla la ma de cada jugador indicant també de qui es
pintaMans mans = do
  putStrLn("## Ma del jugador 1 -> " ++ show (mans!!0))
  putStrLn("## Ma del jugador 2 -> " ++ show (mans!!1))
  putStrLn("## Ma del jugador 3 -> " ++ show (mans!!2))
  putStrLn("## Ma del jugador 4 -> " ++ show (mans!!3))

-- Donat el test (trampa o punts parelles), Les cartes del jugadors, el trumfu, la partida que s'ha jugat i el número de jugador que l'ha començat
-- Pinta per pantalla en un format llegible el resultat d'executar el test.
-- doTest "Test 1" 1 testMans (Pal Oros) test1 2
doTest textTitol numExplicacio mans trumfu partida jugador = do
  putStrLn((titol textTitol))
  putStrLn((capcalera jugador (seguentJugador jugador) (show trumfu)))
  putStrLn(separador)
  putStrLn("## MANS : ")
  putStrLn(separador)
  pintaMans mans
  putStrLn(separador)
  putStrLn("## PARTIDA : ")
  putStrLn(separador)
  pintaPartida partida 0
  putStrLn(separador)
  let infoTrampa = (trampa mans trumfu partida jugador)
  if infoTrampa == Nothing then do
    let (eq1, eq2) =  (\(Just x)->x) (puntsParelles mans trumfu partida jugador)
    putStrLn("## Partida Correcte! ")
    putStrLn("## Punts Equip 1 -> " ++ (show eq1))
    putStrLn("## Punts Equip 2 -> " ++ (show eq2))
  else do
    let (basa, numeroBasa, jugador) = ((\(Just x)->x) infoTrampa)
    putStrLn(mostraTrampa (show basa) numeroBasa jugador)
  putStrLn((explicacioTest numExplicacio))
    --putStrLn("## Hi ha Trampa = " ++ (show basa))  -- show (trampa mans trumfu partida jugador))


----------------------------------------------------------------------------------------------------------------------------------
-- MONADES MENUS
----------------------------------------------------------------------------------------------------------------------------------
-- Pre : True
-- Post : Pinta per pantalla el menú principal
mostraMenu = do
  putStrLn(separador)
  putStrLn("## MENÚ PROGRAMA PRINCIPAL")
  putStrLn(separador)
  putStrLn("## 0 - Finalitzar Programa")
  putStrLn("## 1 - Remenar Baralla")
  putStrLn("## 2 - Repartir")
  putStrLn("## 3 - Testos")
  putStrLn("## 4 - Jugar")
  putStrLn(separador)

-- Pre : True
-- Post : Pinta per pantalla el menú de Testos
mostraMenuTrampa = do
  putStrLn(separador)
  putStrLn("## MENÚ TESTING DE SI HI HA TRAMPES ")
  putStrLn(separador)
  putStrLn("## 0 - Sortir del menu Trampa")
  putStrLn("## 1 - No hi ha error test1")
  putStrLn("## 2 - No hi ha error test2 pal Butifarra")
  putStrLn("## 3 - No hi ha error capot")
  putStrLn("## 4 - Error Falla de gallines")
  putStrLn("## 5 - Error Refalla de gallines") -- Mata Amb trunfu quan encara l'in queden del pal de la basa
  putStrLn("## 6 - Error S'escapen ") --No dona l'As quan l'ha de posar
  putStrLn("## 7 - Error No Mata") -- el jugador no mata quan li toca matar

----------------------------------------------------------------------------------------------------------------------------------
-- MONADES JUGAR PARTIDA
----------------------------------------------------------------------------------------------------------------------------------

pintaPartida partida numeroBasa = do
  if (numeroBasa :: Int) < 12 then do
    putStrLn("##  BASA " ++ (show numeroBasa) ++ " -> " ++ (show (take 4 partida)))
    pintaPartida (drop 4 partida) (numeroBasa + 1)
  else return (1)

pintaPuntsPartida partida punts = do
  putStrLn(separador)
  putStrLn("## PUNTS TOTALS :")
  pintaPartida partida 0
  putStrLn("## EQUIP 1 : " ++ (show (fst(punts))))
  putStrLn("## EQUIP 2 : " ++ (show (snd(punts))))
  putStrLn(separador)
  putStrLn("")
  putStrLn("")


-- Pre : Donada la ma del jugador, el trumfu de la partida la basa actual i si el jugador que ha de tirar es el real
-- Post : Retorna la carta que vol tirar el jugador. Demanant-la al jugador real si es el cas o be fent que el bot en trii una
tiraCarta :: [[Carta]] -> Int -> Int -> Trumfu -> [Carta] -> Bool -> IO (Carta)
tiraCarta mans quiTira quiHaTiratPrimer trumfu basa esJugadorReal = do
  let ma = mans!!quiTira
  if esJugadorReal then do
    putStrLn(separador)
    putStrLn("## Et toca tirar : ")
    putStrLn((mostraBasa quiHaTiratPrimer (montaBasaPerMostrar quiHaTiratPrimer basa) (show trumfu)))
    putStrLn((mostraMa (show ma)))
    putStrLn("## Entra de 0 a n on n és menor al nombre de cartes de la ma: ")
    --opcio <- getLine
  --  let numOp = (read opcio :: Int)
  --  partida++[numOP]
    numOp <- (llegirOpcio ((length ma)-1))
    let carta = (ma!!numOp)
    return (carta)
  else do
    return (tiraCartaBot ma trumfu basa)

completaBasaAmbNulls :: [String] -> [String]
completaBasaAmbNulls x
  | (length x) == 4 = x
  | otherwise = completaBasaAmbNulls (x ++ ["  "])


mouCartesAlFinal :: Int -> [String] -> [String]
mouCartesAlFinal 0 ll = ll
mouCartesAlFinal pos (x:xs) = mouCartesAlFinal (pos - 1) (xs ++ [x])

montaBasaPerMostrar :: Int -> [Carta] -> [String]
montaBasaPerMostrar quiTira cartes = mouCartesAlFinal (4 - quiTira) cartesComStringBasaCompleta
  where
    cartesComStrings = [(show y) | y <- cartes ]
    cartesComStringBasaCompleta = completaBasaAmbNulls cartesComStrings

llegirOpcio :: Int -> IO (Int)
llegirOpcio maxim = do
  opcio <-  getLine
  let numOp = (read opcio :: Int)
  if (numOp < 0) || (numOp > maxim) then do
    putStrLn("T'has passat animal!!! Tria bé!!")
    llegirOpcio maxim
  else
    return (numOp)

-- Pre : Donades les mans dels jugadors, el trumfu, una llista amb l'ordre de tirada dels jugadors, la partida actual i el numero del jugador real [0-3]
-- Post : Fa les accions pertinents per jugar cada mà. Retorna les cartes en ordre que s'han tirat durant la partida
jugar :: [[Carta]] -> Trumfu -> [Int] -> [Carta] -> Int -> IO ([Carta])
jugar [[],[],[],[]] _ _  partida _ = do return (partida)
jugar mans trumfu llistaJugadors partida playerReal = do
  let quiTira = (llistaJugadors!!(mod (length partida) 4))
  let quiTirara = (seguentJugador quiTira)
  let basa = lastN (mod (length partida) 4) partida
  carta <- (tiraCarta mans quiTira (head llistaJugadors) trumfu basa (quiTira==playerReal))
  let novaBasa = basa++[carta]
  if (llistaJugadors!!3) == quiTira then do
    let guanyador = properATirar novaBasa trumfu (head llistaJugadors)
    -- putStrLn( "La basa final es " ++ (show novaBasa) ++ " i el que ha guanyat es " ++ (show guanyador))
    putStrLn(separador)
    putStrLn("## La basa ha quedat : ")
    putStrLn((mostraBasa quiTira (montaBasaPerMostrar (head llistaJugadors) novaBasa) (show trumfu)))
    jugar (extreu mans (novaBasa) (head llistaJugadors)) trumfu (ronda guanyador) (partida++[carta]) playerReal
  else do
    jugar mans trumfu llistaJugadors (partida++[carta]) playerReal

-- Pre : Donada la ma del jugador, si el jugador es el real o no, i el multiplicador actual
-- Post : Retorna Cert si el jugador actual diu que vol Contrar, Recontrar o fer Sant Vicenç. Si el jugador es el real li demanara per tecalt.
escullContro :: [Carta] -> Bool -> Int -> IO (Bool)
escullContro ma esReal multiplicador = do
  if esReal then do
    putStrLn("## Entra un 1 si vols que valgui per 2")
    putStrLn(mostraMa (show ma))
    --opcio <- getLine
    numOp <- llegirOpcio 1
    --let numOp = (read opcio :: Int)
    return (numOp == 1)
  else
    return (pucContrar ma)

-- Pre : Donades les mans dels jugadors, el jugador que ha de contrar [0-3], el jugador real[0-3], el multiplicador actual i el nombre de jugadors que estan d'acrod amb el que s'ha dit
-- Post : Retorna el multiplicador de la partida segons si els jugadors han Contrat, Recontrat o han fet Sant Vicenç
rodaContrar :: [[Carta]] -> Int -> Int -> Int -> Int -> IO (Int)
rodaContrar mans quiContra jugadorReal multiplicador accepten = do
  if (multiplicador == 8) || (accepten == 2) then do return (multiplicador)
  else do
    haContrat <- escullContro (mans!!quiContra) (quiContra == jugadorReal) multiplicador
    if haContrat then do
      rodaContrar mans (seguentJugador quiContra) jugadorReal ((*) multiplicador 2) 0
    else
      rodaContrar mans (seguentJugador (seguentJugador quiContra)) jugadorReal multiplicador (accepten + 1)


-- Pre : Donats els punts de la partida (pot ser nothing) els punts actuals dels equips, el multiplicador de la partida, el trumfu, les cartes que s'han tirat fins ara i el jugador que ha sortit primer de la partida
-- Post : Retorna els punts totals guanyats fins ara sumats als punts de la partida actual. Es te en compte si hi ha agut trampa (Renuncio)
generaResultat :: Maybe (Int, Int) -> (Int, Int) -> Int ->  [[Carta]] -> Trumfu -> [Carta] -> Int -> IO ((Int, Int))
generaResultat puntsPartida punts multiplicador mans trumfu partida quiSurt = do
  if puntsPartida == Nothing then do
    let hiHaTrampa = trampa mans trumfu partida quiSurt
    -- putStrLn("## RENUNCIO!!!!\n " ++ (show hiHaTrampa))
    let (basaTrampa, posicioBasa, trampos) = (\(Just x)->x) hiHaTrampa
    putStrLn(mostraTrampa (show basaTrampa) posicioBasa trampos)
    -- let trampos = ((\(Just (basa,posicio,jugador))->jugador) hiHaTrampa)
    if (mod trampos 2) == 0 then
      return (sumaResultat punts (0,36) 1)
    else
      return (sumaResultat punts (36,0) 1)
  else do
    let aSumar = (\(Just t)->t) puntsPartida
    return (sumaResultat punts aSumar multiplicador)


-- Pre : Donades les mans dels jugadors, el num de jugador que decideix [0-3], el jugador real [0-3] i si s'ha delegat
-- Post : Retorna el trumfu que ha decidit el jugador que li toca o be nothing si creu que no pot fer trumfus i pot delegar
decidirTrumfu :: [[Carta]] -> Int -> Int -> Bool -> IO (Maybe (Trumfu))
decidirTrumfu mans quiDecideix jugadorReal saDelegat = do
  let company = (seguentJugador (seguentJugador (quiDecideix)))
  if quiDecideix == jugadorReal then do
    --mostraMa (show (mans!!quiDecideix))
    putStrLn(mostraMa (show (mans!!quiDecideix)))
    putStrLn("## 1 -> Oros")
    putStrLn("## 2 -> Copes")
    putStrLn("## 3 -> Espases")
    putStrLn("## 4 -> Bastos")
    putStrLn("## 5 -> Butifarra")
    if saDelegat then do
      putStrLn("## Escull trufmu : ")
    else do
      putStrLn("## 6 -> Delega")
      putStrLn("## Escull trufmu o delega : ")
    --mostraOpcionsTrumfu saDelegat -- MOstra les 6 opcions
    --opcio <- getLine
    --let trumfuEsc = read opcio
    trumfuEsc <- llegirOpcio (if saDelegat then 5 else 6)
    if (trumfuEsc >= 1) && (trumfuEsc <=4) then return (Just (Pal (toEnum(trumfuEsc - 1))))
    else if trumfuEsc == 5 then return (Just Butifarra)
    else do
      if saDelegat then error "Havies d'escollir si o si "
      else do
        trumfu <- (decidirTrumfu mans company jugadorReal True)
        return (trumfu)
  else do
    let trumfu = (escullTrumfu (mans!!quiDecideix) saDelegat)
    if trumfu == Nothing then do
      if company == jugadorReal then do
        trumfuDecidit <- (decidirTrumfu mans company jugadorReal True)
        return (trumfuDecidit)
      else return (escullTrumfu (mans!!quiDecideix) True)
    else return (trumfu)

-- Pre : Donades la baralla, els punts dels equips en forma de tupla de enters
-- Post : va generant mans fins que un equip arriva a 101 i per tant guanya la partida.
partidaNova barallaCartes punts jugadorBarreja ra jugadorReal = do
  putStrLn (separador)
  if fst(punts) >= 101 then
      putStrLn("## HA GUANYAT L'EQUIP 1 QUE CONSTA DELS JUGADORS 0 i 2")
  else if snd(punts) >= 101 then
      putStrLn("## HA GUANYAT L'EQUIP 2 QUE CONSTA DELS JUGADORS 1 i 3")
  else do
    putStrLn ("## Comencem partida")
    putStrLn (separador)
    let quiReparteix = (seguentJugador jugadorBarreja)
    let quiSurt = (seguentJugador quiReparteix)
    let mans = (reparteix [[],[],[],[]] (barreja barallaCartes ra) quiReparteix)
    trumfuDecidit <- decidirTrumfu mans quiReparteix jugadorReal False
    let trumfu = (\(Just x)->x) trumfuDecidit
    putStrLn("## S'ha fet trumfus : " ++ (show trumfu))
    multiplicador <- rodaContrar mans quiSurt jugadorReal (if trumfu == Butifarra then 2 else 1) 0
    putStrLn("## S'aplicarà el multiplicador : " ++ (show multiplicador))
    partida <- jugar mans trumfu ((ronda quiSurt)) [] jugadorReal
    resultatFinal <- generaResultat (puntsParelles mans trumfu partida quiSurt) punts multiplicador mans trumfu partida quiSurt
    pintaPuntsPartida partida resultatFinal
    partidaNova barallaCartes resultatFinal (seguentJugador jugadorBarreja) ra jugadorReal

----------------------------------------------------------------------------------------------------------------------------------
-- Programa Principal
----------------------------------------------------------------------------------------------------------------------------------

menuTrampes = do
  mostraMenuTrampa
  putStrLn(separador)
  --opcio <- getLine
  --let numOpcio = read opcio
  numOpcio <- llegirOpcio 6
  if numOpcio == 0 then do
    putStrLn("Retrocedir")
  else if numOpcio == 1 then do
    doTest "Test 1" 1 testMans (Pal Oros) test1 2
    menuTrampes
  else if numOpcio == 2 then do
    doTest "Test 2" 2 testMans Butifarra test2 1
    menuTrampes
  else if numOpcio == 3 then do
    doTest "Test 3" 3 testMans (Pal Bastos) capot 3
    menuTrampes
  else if numOpcio == 4 then do
    doTest "Falla de Gallines" 4 testMans (Pal Oros) fallaGallina 2
    menuTrampes
  else if numOpcio == 5 then do
    doTest "Realla de Gallines" 5 testMans (Pal Oros) refallaGallina 2
    menuTrampes
  else if numOpcio == 6 then do
    doTest "S'escapen" 6 maSescapa (Pal Oros) sescapa 2
    menuTrampes
  else do
    putStrLn("L'has cagat. Tria bé coi!!")
    menuTrampes


programa barallaCartes ra = do
  --Generar Baralla
  mostraMenu
  --opcio <- getLine
  --let numOpcio = read opcio
  numOpcio <- llegirOpcio 4
  if numOpcio == 0 then
    putStrLn("Numero 0")
  else if numOpcio == 1 then do
    --Remenar la baralla
    putStrLn(separador)
    putStrLn("## Hem remenat la baralla!")
    let barrejades = (barreja barallaCartes ra)
    putStrLn(show barrejades)
    programa barrejades ra
  else if numOpcio == 2 then do
    -- Repartir cartes
    putStrLn(separador)
    putStrLn(show barallaCartes)
    putStrLn("## Entra un jugador de 0 a 3 que serà el que rebrà la primera.")
    --jug <- getLine
    --let numJug = read jug
    numJug <- llegirOpcio 3
    let mans = reparteix [[],[],[],[]] barallaCartes numJug
    putStrLn(show mans)
    programa barallaCartes ra
  else if numOpcio == 3 then do
    --Trampa
    menuTrampes
    programa barallaCartes ra
  else if numOpcio == 4 then do
    partidaNova barallaCartes (0,0) 0 ra 0
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
