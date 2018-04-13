module Drawable where

titol :: String -> String
titol t = separador ++ "\n## "++ t ++ "\n" ++ separador

capcalera :: Int -> Int -> String -> String
capcalera quiReparteix quiSurt trumfus = "## reparteix : " ++ (show quiReparteix) ++ "\n## Surt : " ++ (show quiSurt) ++ "\n## Fa trumfus : " ++ (show quiReparteix) ++ "\n## Trumfus : " ++ trumfus

separador :: String
separador = "################################################################"

explicacioTest :: Int -> String
explicacioTest numeroTest
  | numeroTest == 1 = "## La partida acaba sense trampa, l'equip 1 format per els \n"  ++
                      "## jugadors J0 i J2 perden la partida."
  | numeroTest == 2 = "## La partida acaba sense trampa l'equip 1 format per els \n" ++
                      "## jugadors J0 i J2 guanyen la partida i fan 8 punts."
  | numeroTest == 3 = "## L'equip 1 li fot un capot de campionat a l'equip 2, \n" ++
                      "## Un dels integrants de l'equip 2 fa bejenades."
  | numeroTest == 4 = "## El jugador que fa trampes no falla quan ha de fallar!!!"
  | numeroTest == 5 = "## El jugador que fa trampes no Refalla quan ha de Refallar!!!"
  | numeroTest == 6 = "## En aquest test es comprova la obligació de matar, com que \n" ++
                      "## no mata falla per aquí, també es comprova que no tira la \n" ++
                      "## carta que li pertoca"

mostraMa :: String -> String
mostraMa linia
  | (length linia) > 64 = (drop 1 linia)
  | (length linia) == 64 = linia
  | otherwise = mostraMa ("-" ++ linia ++ "-")

montaLinia :: Int -> Int -> Int -> String -> String -> String -> String
montaLinia numLinia jugador1 jugador2 carta1 carta2 trumfu
  | numLinia == 1 = "# QUI SURT : " ++ (show jugador1) ++"             JUGADOR " ++ (show jugador2) ++ "                           #"
  | numLinia == 2 = "#                             " ++ carta1 ++ "                               #"
  | numLinia == 3 = "# JUGADOR "++ (show jugador1)++ "           "++carta1++"               "++carta2++"            JUGADOR "++(show jugador2)++" #"
  | numLinia == 4 = "#                             "++carta1++"                               #"
  | numLinia == 5 = "#                          JUGADOR "++(show jugador1)++"                           #"
  | numLinia == 6 = "#                                                   TRUMFU : "++trumfu++" #"

mostraBasa :: Int -> [String] -> String -> String
mostraBasa quiSurt cartes trumfu =
  separador ++ "\n" ++
  (montaLinia 1 quiSurt  2 "" "" "") ++ "\n" ++
  (montaLinia 2 0 0 (cartes!!2) "" "") ++ "\n" ++
  (montaLinia 3 3 1 (cartes!!3) (cartes!!1) "") ++ "\n" ++
  (montaLinia 4 0 0 (cartes!!0) "" "") ++ "\n" ++
  (montaLinia 5 0 0 "" "" "") ++ "\n" ++
  (montaLinia 6 0 0 "" "" trumfu) ++ "\n" ++
  separador

mostraTrampa :: String -> Int -> Int -> String
mostraTrampa basa numeroBasa jugador =
  mostraMa "RENÚNCIO!!" ++ "\n" ++
  "## S'ha trobat trampa a la basa " ++ (show basa) ++ " amb numero " ++ (show numeroBasa) ++ "\n" ++
  "## El jugador que ha fet la trampa és : " ++ (show jugador)
