-- Constants
cartes1 = [
  [Carta Manilla Bastos, Carta Vuit Bastos, Carta Tres Espases, Carta As Copes, Carta Quatre Bastos, Carta Cavall Espases, Carta Set Copes, Carta As Oros, Carta Cinc Bastos, Carta Sota Copes, Carta Quatre Espases, Carta Set Bastos],
  [Carta Sota Bastos, Carta Cavall Bastos, Carta Manilla Espases, Carta Vuit Copes, Carta Cinc Oros, Carta Vuit Espases, Carta Manilla Copes, Carta Sis Oros, Carta Sota Oros, Carta Cinc Copes, Carta Set Oros, Carta Quatre Copes],
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

-- Pre : Filtra les cartes d'un pal en concret
-- Post : Cartes de pal PalDemanat o llista buida.
cartesPal :: [Carta] -> Pal -> [Carta]
cartesPal ll palDemanat = [ (Carta t pal) | (Carta t pal) <- ll, pal == palDemanat ]
-- cartesPal ll palDemanat = filter ((Carta  palDemanat)==) ll

-- Pre : Llista != []
-- Post : Retorna el pal guanyador donat llista de cartes i trumfo
palGuanyadorBasa :: [Carta] -> Pal -> Pal
palGuanyadorBasa [] trumfo = error "No em pots passar una llista buida animal! "
palGuanyadorBasa ll trumfo = if length (cartesPal ll trumfo) > 0 then trumfo else head [ pal | (Carta t pal) <-ll ]

-- Pre : Llista != []
-- Post : La carta i la posició guanyadora en forma de tupla
quiGuanya :: [Carta] -> Pal -> (Carta, Int)
quiGuanya [] trumfo = error "No em pots passar una llista buida Animal! "
quiGuanya ll trumfo =  (cartaGuanyadora, (head [index | (index, carta) <- zip [1..] ll, carta == cartaGuanyadora]))
  where
    palGuanyador = palGuanyadorBasa ll trumfo
    cartaGuanyadora = maximum (cartesPal ll palGuanyador)

-- findPos list elt = [index | (index, e) <- zip [0..] list, e == elt]
