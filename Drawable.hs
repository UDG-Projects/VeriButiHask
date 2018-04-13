module Drawable where

titol :: String -> String
titol t = separador ++ "\n## "++ t ++ "\n" ++ separador

capcalera :: Int -> Int -> String -> String
capcalera quiReparteix quiSurt trumfus = "## reparteix : " ++ (show quiReparteix) ++ "\n## Surt : " ++ (show quiSurt) ++ "\n## Fa trumfus : " ++ (show quiReparteix) ++ "\n## Trumfus : " ++ trumfus

separador :: String
separador = "################################################################"

explicacioTest :: Int -> String
explicacioTest numeroTest
  | numeroTest == 1 = "La partida acaba sense trampa, l'equip 1 format per els jugadors J1 i J3 perden la partida."
  | numeroTest == 2 = ""
  | numeroTest == 3 = ""
  | numeroTest == 4 = ""
  | numeroTest == 5 = ""
  | numeroTest == 6 = ""
  | numeroTest == 7 = ""

mostraMa :: String -> String
mostraMa ma = "## " ++ ma

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
  separador ++ "\n"
