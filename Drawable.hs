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
