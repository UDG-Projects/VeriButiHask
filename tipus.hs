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


partida1=[Carta Manilla Bastos, Carta Sota Bastos, Carta As Bastos, Carta Dos Bastos,
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

-- Pre : 0 < [x && y] < 5
-- Post : [1-4] referent al proper a tirar.
quiSortira :: Int -> Int -> Int
quiSortira x y
  | modul == 0 = 4
  | otherwise = modul
  where
    modul = ((mod) ((-) ((+) x y) 1) 4)

-- Pre :
-- Post :
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

jugades :: [Carta] -> Trumfu -> [Carta] -> [Carta]
jugades cartesJugador _ [] = cartesJugador
jugades cartesJugador trumfu ll =
  if (\(Carta tc pal)->pal== palBasa) (fst guanyador) then
    if (length cartesJugadorPalBasa) > 0 then
      if esticObligatAMatar && (length (cartesJugadorMatenPalBasa) > 0) then cartesJugadorMatenPalBasa
      else cartesJugadorPalBasa
    else
      if esticObligatAMatar && ((length cartesJugadorMaten) > 0) then cartesJugadorMaten
      else cartesJugador
  else
    if (length cartesJugadorPalBasa) > 0 then cartesJugadorPalBasa
    else
      if esticObligatAMatar && (length cartesJugadorMaten) > 0 then cartesJugadorMaten
      else cartesJugador
  where
    guanyador = quiGuanya ll trumfu
    esticObligatAMatar = saDeMatar (snd guanyador) ((length ll) + 1)
    palBasa = ((\(Carta tc pal)->pal) (head ll))
    cartesJugadorPalBasa = cartesPal cartesJugador palBasa
    cartesJugadorMaten = filter (mata trumfu (fst guanyador)) cartesJugador
    cartesJugadorMatenPalBasa = cartesPal cartesJugadorMaten palBasa


-- Pre :
-- Post :
-- trampa :: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe ([Carta],Int, Int)
-- trampa ll trumfo jugades jug = Nothing

-- Pre :
-- Post :
--jugades :: [Carta] -> Trumfu -> [Carta] -> [Carta]
--jugades cartesJugador _ [] = cartesJugador
--jugades cartesJugador (Pal trumfu) [(Carta tc p)] =
--  if length (cartesPal cartesJugador p) > 0  then
--    if length (filter (\carta -> carta > (Carta tc p)) (cartesPal cartesJugador p)) > 0 then filter (\carta -> carta > (Carta tc p)) (cartesPal cartesJugador p)
--    else cartesPal cartesJugador p
--  else if length (cartesPal cartesJugador trumfu) > 0 then cartesPal cartesJugador trumfu
--  else cartesJugador
--jugades cartesJugador (Pal trumfu) [(Carta tc1 p1), (Carta tc1, p2)] =
--  if length (cartesPal cartesJugador p1) > 0  then
--    if length (filter (\carta -> carta > (Carta tc p1)) (cartesPal cartesJugador p1)) > 0 then filter (\carta -> carta > (Carta tc1 p1)) (cartesPal cartesJugador p1)
--    else cartesPal cartesJugador p
--  else if length (cartesPal cartesJugador trumfu) > 0 then
--    if length (filter (\carta -> carta > (Carta tc p)) (cartesPal cartesJugador trumfu)) > 0 then filter (\carta -> carta > (Carta tc p)) (cartesPal cartesJugador trumfu)
--    else cartesPal cartesJugador trumfu
--  else cartesJugador
--jugades cartesJugador (Butifarra) ((Carta tc p):xs) = if length mateixPal > 0 then mateixPal else cartesJugador  -- Falta definir filtre mateixpal
--  where
--    mateixPal = cartesPal cartesJugador p
--jugades cartesJugador (Pal trumfu) ((Carta tc p):xs) = if length mateixPal > 0 then if (snd guanyadorActual) ==
--  where
--    guanyadorActual = quiGuanya basa trumfu
--    mateixPal = cartesPal cartesJugador p
--    trumfos   = cartesPal cartesJugador trumfu
--(Carta tc p)
