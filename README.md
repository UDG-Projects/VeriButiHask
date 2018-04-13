---
title: "Verificador Butifarra Haskell - Pràctica Funcional"
author: [Francesc Xavier Bullich Parra i Marc Sànchez Pifarré, GEINF (UDG-EPS)]
date: 2018-04-13
subject: "Udg - Eps"
tags: [Butifarra, Haskell]
subtitle: "Tutor de la pràctica : Mateu Villaret"
titlepage: true
titlepage-color: 06386e
titlepage-text-color: FFFFFF
titlepage-rule-height: 4
...

# Informe sobre la pràctica funcional.

En aquest informe s'hi allotja tot el codi degudament documentat sobre la pràctica funcional anomenada com veributihask. Aquesta pràctica consta de dues parts diferenciades, una primera part obligatòria en la que es demana una série de funcions explicades amb detall més endavant i una segona part optativa que extén la part obligatòria i que també està explicada a posteriori.

## Índex :

1. Explicació de la pràctica
    * 1.1. Part obligatòria
    * 1.2. Part opcional
2. Explicaió del codi
    * 2.1. Constants
    * 2.2. Tipus
    * 2.3. Funcions Destacades
    * 2.4. Intent de IA
    * 2.5. Mònades
    * 2.6. Fitxer Drawable
3. Programa
    * 3.1. Funcionament del programa principal
    * 3.2. Funcionament del programa de Test
4. Exemples d'execució
    * 4.1 Pantallassos
    * 4.2 Exemple de partida
5. Conclusions

## 1.Explicació de la pràctica.

En el nostre cas s'ha optat per realitzar una pràctica al complert, és a dir no ens hem limitat només a fer un verificador de butifarra en haskell sinó que hem anat més enllà i hem realitzat un programa capaç de :

- Generar baralles de cartes
- Barrejar baralles de cartes
- Repartir una baralla de cartes (amb l'algoritme de la butifarra)
- Generar partides de butifarra mitjançant la IA que es proposa (tirant la carta més alta de la mà)
- Verificar que una partida de butifarra s'ha jugat bé i sense trampa.
- Contar punts d'una partida
- Jugar a la butifarra contra la IA proposada (tirant la carta més alta de la mà)

### 1.1.Part obligatòria

Com a part obligatòria hem ralitzat totes les funcions que s'han demanat.

#### Trampa

```haskell
-- Pre : Donades les mans dels jugadors, el trumfu de la partida, la partida sencera i qui ha començat jugant
-- Post : Retorna Nothing si no hi ha hagut trmapa o Just (basa on hi ha la trampa, numero de basa, jugador que ha tirat la carta)
trampa :: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe ([Carta],Int, Int)
```

#### CartesGuanyades

```haskell
-- Pre : Donat el trumfu la partida i el jugador que ha començat [0-3] (S'ha d'haver jugat la partida sencera)
-- Post : Retorna les cartes guanyades de cada equip en forma de tupla ([cartes equip 1], [cartes equip 2])
cartesGuanyades::  Trumfu -> [Carta] -> Int -> ([Carta],[Carta])
```

#### Punts

```haskell
-- Pre : Donades una llista de cartes
-- Post : Retorna la suma dels punts de les cartes de la llista sumant 1 punt per basa
punts :: [Carta] -> Int
```

#### PuntsParelles

```haskell
-- Pre : Donades les mans dels jugadors, el trumfu, la partida i el jugador que ha començat la partida [0-3]
-- Post : Retorna nothing si s'ha fet trampa, o (Punts equip 1, Punts equip 2) en cas que no s'hagi fet trampa
puntsParelles :: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe (Int, Int)
```

### 1.2.Part opcional

Com a part opcional hem realitzat totes les parts a la manera que les hem interpretat. M'explico :

**Contempleu el cas de cantar Butifarra.**

Sobre aquest cas no hi ha res a discutir, quan es juga el jugador pot cantar butifarra. Les IA poden realitzar butifarra sempre que tinguin 2 manilles i 1 as.

**Considereu partides senceres amb les opcions de contrar, etc.**

Sobre aquesta part hem interpretat que és com un pas previ al fet de fer el programa interactiu i hem integrat el seu desenvolupament dins del programa interactiu que participa amb l'usuari. El verificador de trampes i el contador de punts no tenen la funcionalitat del contro ni evaluen partides senceres, s'ha realitzat en una altre funció que crida al conta punts.

**Feu un programa interactiu per a jugar a la Butifarra amb tres robots (si esteu interessats amb aquest apartat parleu amb mi).**

Vam parlar amb tu i hem fet el programa. Per poder jugar-hi carrega el veributihask.hs, escriu main i segueix les instruccions! Enjoy it!


## 2.Explicaió del codi

En aquest apartat s'introdueix tot el codi de la pràctica separat per blocs i es farà ressó o es comentarà més detenidament els aspectes més delicats o les funcions més enrevessades que hi puguin sortir. Cal comentar que el programa consta de dos fitxers, un fitxer on hi ha tot el codi referent a la butifarra i un intent de mòdul per poder pintar per pantalla les diferents accions de la butifarra anomenat drawabe.hs.

Òbviament hi ha a sobre de tot dels fitxers els corresponents imports o declaracions de mòduls.

```haskell
import System.Random
import Drawable
```

### 2.1. Constants

En aquest programa per facilitat l'execució s'ha integrat un grup de constants a la part superior del fitxer amb el format adient per permetre una execució de les diferents formes possibles de les funcions esmentades anteriorment. Per tant ens permet fer un petit test de si les funcions que s'han comentat anteriorment funcionen o si no funcionen.

**Per tant podem realitzar coses com la següent:**

```haskell
Prelude> :l veributihask.hs
[1 of 2] Compiling Drawable         ( Drawable.hs, interpreted )
[2 of 2] Compiling Main             ( veributihask.hs, interpreted )
Ok, two modules loaded.
*Main> trampa testMans Butifarra test2 1
Nothing
```

#### Constants disponibles :

```haskell

-- MANS
-----------------------------------------------

testMans = [
  [Carta Cavall Espases, Carta Rei Oros, Carta Quatre Espases, Carta Set Copes, Carta As Espases, Carta Manilla Espases, Carta Manilla Bastos, Carta Sota Oros, Carta Vuit Copes, Carta As Copes, Carta Cinc Oros, Carta Sota Bastos],
  [Carta Dos Copes, Carta Quatre Copes, Carta Manilla Oros, Carta Cavall Oros, Carta Rei Copes, Carta Cinc Espases, Carta Vuit Espases, Carta Dos Oros, Carta As Oros, Carta Sota Copes, Carta Sis Bastos, Carta Quatre Oros],
  [Carta Vuit Bastos, Carta Set Espases, Carta Set Oros, Carta Tres Copes, Carta Sota Espases, Carta Tres Bastos, Carta Set Bastos, Carta Vuit Oros, Carta Cinc Copes, Carta As Bastos, Carta Dos Bastos, Carta Rei Bastos],
  [Carta Sis Oros, Carta Tres Espases, Carta Tres Oros, Carta Manilla Copes, Carta Cavall Copes, Carta Rei Espases, Carta Cavall Bastos, Carta Dos Espases, Carta Sis Espases, Carta Quatre Bastos, Carta Sis Copes, Carta Cinc Bastos]]

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


-- PARTIDES
-----------------------------------------------
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
```

### 2.2. Tipus

S'ha optat per realitzar els tipus justos per poder montar les cartes ja que hem treballat amb llistes de cartes tota l'estona i no hem vist la necessitat d'incorporar-ne més.

#### Pal
El tipus pal és simple, pot esdevenir 4 coses, oros, copes espases o bastos. En aquest cas el que cal destacar és el deriving Enum. Ja que de manera intencionada Oros tindrà un fromEnum = 0, copes = 1, espases = 2 i bastos = 3. Més endavant al tipus Carta s'especifica el perquè d'aquest fet.

També destaquem que no es fa un deriving show ja que ens interessa mostrar el pal d'una altre manera.

```haskell
data Pal = Oros | Copes | Espases | Bastos deriving (Eq, Enum)
instance Show Pal where
  show Oros = "O"
  show Copes = "C"
  show Espases = "E"
  show Bastos = "B"
```

#### Trumfu

El trumfu és la combinació o bé de un pal o bé Butifarra, Per tant el constuctor Butifarra generarà un tipus concret i en cas que no sigui Butifarra el seu constructor s'anomenarà Pal.

```haskell
data Trumfu = Butifarra | Pal Pal deriving (Eq)
instance Show Trumfu where
  show (Butifarra) = "#"
  show (Pal pal)   = show pal
```

Exemple de generació d'un trumfu :

```haskell
*Main> (show (Pal Oros))
"O"
```

#### TipusCarta

Reconec que el nom ha quedat un pél ambigu, el que es vol aconseguir és un tipus que dongui valor a una carta. En aquest cas com en el cas vist anteriorment "Pal" també li donem rellevància al deriving Enum tal com al deriving Ord. En l'explicació del tipus "Carta" es comenta el perqué.

De la mateixa manera que en el Pal s'ha sobreescrit la instància de show per facilitar la realització del programa. En el show d'un nombre que no és ni manilla ni cap figura ja es pot veure una pinzellada del que es comentarà a l'explicació del tipus "Carta".

```haskell
data TipusCarta = Dos | Tres | Quatre | Cinc | Sis | Set | Vuit | Sota | Cavall | Rei | As | Manilla deriving (Eq, Ord, Enum)
instance Show TipusCarta where
  show Sota = "S"
  show Cavall = "C"
  show Rei = "R"
  show As = "A"
  show Manilla = "9"
  show x = show (fromEnum(x) + 2)
```
Així doncs el fromEnum de un Dos serà un 0, el de un Tres serà un 1 i així successivament fins arrivar a la manilla que serà el 11.

#### Carta

Aquest tipus és la base per al funcionament de la baralla.

```haskell
data Carta = Carta TipusCarta Pal deriving (Eq)

instance Show Carta where
  show (Carta a b) = show a ++ show b

instance Ord Carta where
  (Carta te pe) <= (Carta td pd) = te <= td
```

Uneix els tipus TipusCarta i Pal sota un mateix tipus amb el constructor "Carta". Ens permet docns realitzar cartes com per exemple :

```haskell
*Main> Carta Manilla Bastos
9B
```
Tal com he anat comentant anteriorment voldria destacar el instance Enum. Instance Enum parteix de la base que en la baralla espanyola hi ha 48 cartes on cada pal consta de 12 cartes i on cada valor per cada carta donat un pal és de 0 - 11. Per aquest motiu i amb habilitat hem definit el tipus pal amb l'ordre per defecte que s'estipula amb "Oros a la mà" que és (Oros, Copes, Espases i Bastos) i fent que derivin directament de l'enum assignant el valors que ja s'han comentat a la definició del tipus Pal.

S'ha seguit la mateixa estratègia amb el tipus "TipusCarta" ordenant les definicions dels diferents constructors amb l'ordre de menor a major.

```haskell
instance Enum Carta where
  toEnum x = (Carta (toEnum (mod x 12)) (toEnum (div x 12)))
  fromEnum (Carta tipus pal) = ((fromEnum tipus)) + ((fromEnum pal) * 12)
```

Així doncs tenint en compte el que s'ha comentat anteriorment i utilitzant la definició del instance Enum de Carta aconseguim enumerar totes les cartes d'una baralla fent que la primera sigui el Dos d'Oros i que la última sigui la manilla de Bastos.

Ens permet fer seqüències tant elegants com aquestes :

```haskell
*Main> baralla = [(Carta Dos Oros)..(Carta Manilla Bastos)]
*Main> baralla
[2O,3O,4O,5O,6O,7O,8O,SO,CO,RO,AO,9O,2C,3C,4C,5C,6C,7C,8C,SC,CC,RC,AC,9C,2E,3E,4E,5E,6E,7E,8E,SE,CE,RE,AE,9E,2B,3B,4B,5B,6B,7B,8B,SB,CB,RB,AB,9B]
*Main> import System.Random
*Main System.Random> let random = take 200 (randomRs (0 :: Int, 47) seed)
*Main System.Random> barreja baralla random
[8B,9B,SC,4E,4B,3B,3E,3C,3O,AE,5B,9C,AB,4O,8O,7O,7E,5E,RC,RE,2B,4C,6B,SO,8E,6E,8C,5O,CO,6O,AC,7C,SB,CC,RO,9O,SE,RB,CE,9E,2O,6C,2E,2C,7B,CB,AO,5C]
```

### 2.3. Funcions

A continuació es mostren totes les funcions que s'han realitzat per fer la pràctica. Totes elles comentades degudament.

#### Cal destacar :

- **L'ús de funcions d'ordre superior (la funció mata dins dels filtres.)**

Exemple :
```haskell
cartesJugadorMaten = filter (mata trumfu (fst guanyador)) cartesJugador
```
Es pot trobar aquesta línia de codi dins de la funció jugades.

- **L'ús de llistes de comprensió amb els diferents tipus de la llibreria**

Exemple :
```haskell
-- Pre : Doanda la ma del jugador
-- Post : Retorna cert si dins la ma hi ha fallo o semifallo (0 o 1 sola carta d'un pal concret)
tincSemiFalloOFallo :: [Carta] -> Bool
tincSemiFalloOFallo ma = or [ (length y) <= 1 | y <- [ cartesPal ma x | x <- [(Oros)..(Bastos)]]]
```

- **L'ús de les funcions pròpies de les mateixes llistes**

Exemple :
```haskell
novaMans = (take jugador mans) ++ [maJugador] ++ (drop (jugador + 1) mans)
```
Es pot trobar aquesta línia de codi dins de la funció reparteix.

- **Ús del Maybe -> Just | Nothing**

Exemple :
```haskell
-- Pre : Donada una ma i si està o no obligat a fer trumfu
-- Post : retorna nothing si pot no fer-ho i no ho fa o El trumfu escollit.
escullTrumfu :: [Carta] -> Bool -> Maybe (Trumfu)
escullTrumfu ma obligat
  | tincButifarra ma = Just (Butifarra)
  | obligat || (tincSemiFalloOFallo ma) = Just maxPal
  | otherwise = Nothing
  where
    maxPal = (\(Carta tp p)->(Pal p)) (head (snd (maximum [ (length y, y) | y <- [cartesPal ma x | x <- [(Oros)..(Bastos)] ] ] ) ) )
```

#### Genèriques

Ens hem trobat en complicacions a l'hora d'accedir a les llistes per redera i hem trobat quelcom per internet que ens ha ajudat. Per exemple la següent funció.

```haskell
-- Pre : Donat un nombre n i una llista d'elements
-- Post : Retorna els n ultims elements de la llista
lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const . drop 1) xs (drop n xs)

-- No té gaire sentit peró ajuda a minimitzar codi en alguns moments.
-- Pre : Donada una condició i dues llistes
-- Post : Escull l1 si b i l2 si no b
selecciona :: Bool -> [a] -> [a] -> [a]
selecciona b l1 l2 = if b then l1 else l2

```

#### Booleanes

Les següents funcions ens ajuden a decidir comportaments sobre decisions que s'han de prendre en funció de les cartes que es posseeix. Retornen cert o fals i es poden utilitzar conjuntament amb funcions d'ordre superior o filtres.

**La funció mata permet generar comparacions entre dues cartes**

```haskell
-- Pre : Donat el tumfu de la partida i dues cartes Mira si la primera carta mata a la segona
-- Post : Retorna si la primera carta mata a la segona tinguent en compte el trumfu de la partida
mata :: Trumfu -> Carta -> Carta  -> Bool
mata trumfu (Carta tc1 pal1) (Carta tc2 pal2)
  | trumfu == Butifarra = (pal1 == pal2) && ((Carta tc1 pal1) < (Carta tc2 pal2))
  | otherwise = ((pal1 == pal2) && (Carta tc1 pal1) < (Carta tc2 pal2)) || ((pal1 /= pal2) && ((\(Pal p)->p == pal2) trumfu))
```

**Per saber si la carta que estic mirant és una manilla o no, en cas que ho sigui retornarà fals.**

```haskell
-- Pre : Donada una carta
-- Post : Retorna si és possible que hi hagi una carta superior del mateix pal
teCartaMajorDelPal :: Carta -> Bool
teCartaMajorDelPal (Carta x pal)
  | x == Manilla = False
  | otherwise = True
```

**Donades dues posicions a la taula que fan referència a jugadors decidirà si un jugador està obligat a matar la carta que d'un altre mirant si és el seu company o si no.**

```haskell
-- Pre : Donada la posicio del jugador que guanya [0-3] i la posicio del jugador actual [0-3].
-- Post : Retorna cert si el jugador actual ha de matar fals altrament (guanya el company del jugador actual)
saDeMatar :: Int -> Int -> Bool
saDeMatar posGuanya posMeu
  | posMeu - 2 >= 0 = posMeu - 2 /= posGuanya
  | otherwise = True
```
**Definim que el fet de poder contrar resideixi en la possessió de dues manilles**
Aquest mètode es podria extendre i es podria decidir fer-lo més intel·ligent, en tot cas només es centraria en aquest moment a la ma que posseeix el bot, es podria passar el jugador que ha fet trumfus i es podria tenir en compte la posició en la que es troba a la taula. També es podria passar un flag que dictés si el trumfu ha sigut delegat o no.

```haskell
-- Pre : Donada la ma del jugador
-- Post : Retorna cert si dins la ma hi ha les cartes adients com per contrar
pucContrar :: [Carta] -> Bool
pucContrar ma = (length manilles) > 1
  where
    manilles = [ x | x <- ma , (\(Carta tp p) -> tp == Manilla) x ]
```

**Defninim el fet que es posseeixi una butifarra quan el bot té dues manilles i un as independentment de tot lo altre.**

```haskell
-- Pre : Donada la ma del jugador
-- Post : Retorna cert si dins la ma hi ha les cartes adients per cantar Butifarra
tincButifarra :: [Carta] -> Bool
tincButifarra ma = (length asos) >= 1 && (length manilles) >= 2
  where
    asos = [ x | x <- ma , (\(Carta tp p) -> tp == As) x ]
    manilles = [ x | x <- ma , (\(Carta tp p) -> tp == Manilla) x ]
```
**És molt útil saber si es té o no semifallo o fallo directe per poder saber que es volen fer trumfus.**
En aquest cas es smimplifica i es permet fer trumfus només posseint un semifallo sempre que no estigui obligat a fer-ne.

```haskell
-- Pre : Doanda la ma del jugador
-- Post : Retorna cert si dins la ma hi ha fallo o semifallo (0 o 1 sola carta d'un pal concret)
tincSemiFalloOFallo :: [Carta] -> Bool
tincSemiFalloOFallo ma = or [ (length y) <= 1 | y <- [ cartesPal ma x | x <- [(Oros)..(Bastos)]]]
```
### Funcions Proposades


```haskell

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

-- Pre : 0 <= [x && y] < 4 Donat el jugador actual (x) i la posisico del que ha guanyat la basa (y)
-- Post : retorna el num de jugador que ha de començar la seguent basa [0-3].
quiSortira :: Int -> Int -> Int
quiSortira x y = (mod ( x + y)  4)

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
```
Fent l'informe me n'he adonat que no hem fet la funció basaCorrecta i que ens podria haver servit per delimitar una basa com a incorrecte i detectar una trampa al moment en que es juga.

En el nostre cas el basa correcte es troba dins del where de la funció trampa.

### Funcions Obligatòries.

Les següents funcions son les funcions que s'han demanat obligatòriament a la pràctica i també algunes que es poden considerar de més complexitat.

```haskell
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

```


### Funcions en general.

Les següents funcions son utilitzades per a la realització tant de la part opcional com de la part obligatòria de la pràctica.

```haskell

-- Pre : True
-- Post : Retorna una baralla de cartes de Butifarra ordenada del Dos d'oros a la Manilla De Bastos
baralla :: [Carta]
baralla = [(Carta Dos Oros)..(Carta Manilla Bastos)]

-- Pre : Donat el primer en tirar
-- Post : Es genera una llista de 4 enters (jugadors) amb l'ordre de tirarada d'una basa en funció del primer.
ronda:: Int -> [Int]
ronda primer = primer:[ (seguentJugador (primer+x)) | x <-[0..2]]

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

-- Pre : Donat el número d'un jugador
-- Post : Retorna el número del següent jugador
-- Aquest és una de les funcions més importants que hem pogut realitzar ja que ens permet mantenir una roda constant i fer que l'accés als diferents jugadors sigui pràctic.
-- Es planteja un ús com per exemple un successor.
seguentJugador :: Int -> Int
seguentJugador jugador = mod (jugador + 1) 4

-- Pre : Donada una basa, el jugador que l'ha començat i el jugador buscat
-- Post : Retorna la carta que ha jugat el jugador que busquem
cartaJugadorBasa :: [Carta] -> Int -> Int -> Carta
cartaJugadorBasa (carta:pila) comenca jugador
  | comenca == jugador = carta
  | otherwise = cartaJugadorBasa pila (seguentJugador comenca) jugador

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

-- Pre : Donades les mans dels jugadors, la basa i qui ha començat a jugar la basa [0 -3]
-- Post : Retorna les mans dels jugadors sense les cartes que s'han jugat a la basa
extreu :: [[Carta]] -> [Carta] -> Int -> [[Carta]]
extreu mans basa jug = [filter (/=cartaJugadorBasa basa jug x) (mans!!x) | x <- [0..3]]

-- Pre : Donada una basa, el trumfu i el primer que ha jugat
-- Post : Retorna el jugador que començarà la següent basa
properATirar :: [Carta] -> Trumfu -> Int -> Int
properATirar basa trumfu jug = (quiSortira jug (snd (quiGuanya basa trumfu)))

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
generarPartida mans trumfu jug = basa ++ generarPartida (extreu mans basa jug) trumfu (properATirar basa trumfu jug)
  where
    -- Aquest maximum s'ha de canviar per un escull millor tirada
    basa= [ tiraCartaBot  (mans!!(mod (jug + (n-1)) 4)) trumfu (take n basa) | n <-[0..3] ]

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


-- Pre : Donades una llista de cartes representades en strings de 2 caràcters (basa complerta o incomplerta)
-- Post : retorna la basa complerta per poder ser pintada
completaBasaAmbNulls :: [String] -> [String]
completaBasaAmbNulls x
  | (length x) == 4 = x
  | otherwise = completaBasaAmbNulls (x ++ ["  "])

-- Pre : Donat la posició del jugador 0 (al tirar una carta a la basa) i el llistat de cartes representades com strings de 2 caràcters
-- Post : mou les cartes que hi ha per derrera del jugador 0 al final de la llista mantenint l'ordre de tirada.
mouCartesAlFinal :: Int -> [String] -> [String]
mouCartesAlFinal 0 ll = ll
mouCartesAlFinal pos (x:xs) = mouCartesAlFinal (pos - 1) (xs ++ [x])

-- Pre : Donada la posició en que ha tirat el jugador 0 i una basa
-- Post : retorna la basa per poder ser pintada indiferentment del nombre de cartes que tingui.
montaBasaPerMostrar :: Int -> [Carta] -> [String]
montaBasaPerMostrar quiTira cartes = mouCartesAlFinal (4 - quiTira) cartesComStringBasaCompleta
  where
    cartesComStrings = [(show y) | y <- cartes ]
    cartesComStringBasaCompleta = completaBasaAmbNulls cartesComStrings

```

### 2.4. Intent de IA

En aquest apartat s'hi endosa un prototip de codi que es veurà al document, està incomplert i no està optimitzat (reduït). Amb aquesta part es pretenia dotar de IA (una mínima) els bots per que tingués una mica més de gràcia la pràctica. Desgraciadament el temps és el que és i no s'ha pogut realitzar dins del termini establert. No hem eliminat el codi per que si en algun moment ens veiem amb ganes de reemprendre el projecte ha tindrem un petit esquelet de com començar-ho.

En aquest moment s'està fent servir la següent funció que escull la carta més alta de la ma del bot i la llença quan li toca. (Aquesta és la IA que hi ha fins el moment).
```haskell
-- Pre : Donada la ma del Bot, el trumfu de la partida i la basa actual
-- Post : retorna la carta que tirarà el Bot segons l'estat de la basa actual
tiraCartaBot :: [Carta] -> Trumfu -> [Carta] -> Carta
tiraCartaBot ma trumfu basa = maximum (jugades ma trumfu basa)
```

La idea era substituïr el maximum per la funció escullMillorSortida o escullCartaATirar en funció de si el jugador estava sortint o si el jugador estava en mig d'una basa. Com ja he comentat s'han començat a plantejar els casos i no s'ha tingut en compte cap tipus de reducció.

```haskell

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
```

En aquest petit projecte de IA s'hi poden veure coses que designaria com "elegants" sobretot a la funció següent on prenen sentit el Enum dels tipus pal, tipusPal i carta.

```haskell
-- Pre : Donada la partida jugada fins el moment i una carta qualsevol
-- Post : Retorna cert si aquesta carta és ferma, fals altrament. (només mira el pal)
esFerma :: [Carta] -> Carta -> Bool
esFerma partida (Carta Manilla pal) = True
esFerma partida (Carta tipus pal) = and [ elem x partida | x <- [(cartaSeguentMajor (Carta tipus pal))..(Carta Manilla pal)]]
```

### 2.5. Mònades

L'ús de les mònades ha sigut anecdòtic fins que hem arrivat a fer la part opcional de provocar una partida jugada contra la màquina. No ens jutgis sisplau per com s'han realitzat les mònades i per si veus alguna bejenada. Hem aconseguit realitar tot el codi "a contracorrent" i no és la millor part de la pràctica peró si una de divertida de crear.


```haskell

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
    opcio <- getLine
    let numOp = (read opcio :: Int)
  --  partida++[numOP]
    let carta = (ma!!numOp)
    return (carta)
  else do
    return (tiraCartaBot ma trumfu basa)


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
    opcio <- getLine
    let numOp = (read opcio :: Int)
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
    opcio <- getLine
    let trumfuEsc = read opcio
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

-----------------------------------------------
-- MONADES TESTING
-----------------------------------------------

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


-----------------------------------------------
-- MONADES MENUS
-----------------------------------------------
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

-----------------------------------------------
-- MONADES JUGAR PARTIDA
-----------------------------------------------

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

-----------------------------------------------
-- Programa Principal
-----------------------------------------------

menuTrampes = do
  mostraMenuTrampa
  putStrLn(separador)
  opcio <- getLine
  let numOpcio = read opcio
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
  opcio <- getLine
  let numOpcio = read opcio
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
    jug <- getLine
    let numJug = read jug
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
```

### 2.6. Fitxer Drawable

En aquest fitxer s'hi ha declarat un mòdul que monta el que serien la majoria de sortides del programa principal. La seva funció és que donats uns strings o uns ints en concret monti strings en un format adient per ser pintats per pantalla.

No té secret i no està documentat per que és molt evident el que fa. No hi ha cap més dificultat que el quadrar la sortida. Com a molt, quelcom a destacar pot ser la funció ma que recursivament centra la ma dins d'un requadre de 64 caselles.

```haskell
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
```

## 3. Programa

S'ha fet un programa principal senzill que només sap llegir números peró que sap fer moltes coses en funció de la combinació dels números que se li entren.

Per executar el programa principal només s'ha de carregar la llibreria "veributihask.hs" i escriure la paraula main.

Exemple :
```haskell
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
Prelude> :l veributihask.hs
[1 of 2] Compiling Drawable         ( Drawable.hs, interpreted )
[2 of 2] Compiling Main             ( veributihask.hs, interpreted )
Ok, two modules loaded.
*Main> main
################################################################
## MENÚ PROGRAMA PRINCIPAL
################################################################
## 0 - Finalitzar Programa
## 1 - Remenar Baralla
## 2 - Repartir
## 3 - Testos
## 4 - Jugar
################################################################
```

Com es pot veure a l'anterior exemple el programa consta de 4 parts, quan es crida al main, aquest crida al programa principal amb una baralla de cartes generada per ordre utilitzant una llista generadora.

**Remenar Baralla**

Donada la baralla de cartes que ha rebut el programa principal la remena i la mostra per pantalla.

**Repartir**

Mostra la baralla de cartes sense repartir i les mans que s'han repartit, serveix per comprovar que es realitza l'algoritme de repartició de la butifarra (no s'ha implementat la part de tallar).

**testos**

Aquest apartat ens porta a un submenu on hi ha programats una série de testos específics que criden a la funció trampa i que proven com funciona la mateixa sotmesa a uns diferents casos. L'exemple d'execució que pots implícit el programa de testos està generat per nosaltres i els casos intenten simular casos reals de partides (les hem jugat nosaltres pot ser que no siguin les millors bases del mon).

**jugar**

Permet realitzar una partida interactiva amb els bots proposats per nosaltres (aquests bots son de lo més idiota que hi ha i poden fer perdre els nervis a qualsevol company). La IA com ja s'ha comentat anteriorment tirarà **sempre** la carta més alta que tingui.

### 3.1. Funcionament del programa principal

El programa principal i tots els derivats només llegeixen números. Per tant sempre s'han d'entrar valors numèrics i en cas que s'entri quelcom que no sigui un valor numèric el programa fallarà i deixarà de funcionar exportant una excepció.

Per seleccionar el valor numèric en tot moment estan indicades les diferents opcions disponibles. En cas que la opció escollida sigui la de jugar el mateix joc ja va indicant quines son les accions que es poden realitzar. Recordar que les mònades d'entrada que s'han programat no accepten valors que no siguin numèrics!

**Exemple d'execució del programa de principal :**

```haskell
################################################################
## MENÚ PROGRAMA PRINCIPAL
################################################################
## 0 - Finalitzar Programa
## 1 - Remenar Baralla
## 2 - Repartir
## 3 - Testos
## 4 - Jugar
################################################################
1
################################################################
## Hem remenat la baralla!
[CC,4O,5O,AB,4C,RC,AO,SO,RE,3O,9B,RB,AC,9C,3E,7O,AE,4E,6C,5B,9E,8O,6O,9O,5C,2O,4B,8C,SC,5E,2B,8E,2C,RO,SE,2E,CB,CE,6B,3B,8B,7E,3C,7B,CO,6E,SB,7C]
################################################################
## MENÚ PROGRAMA PRINCIPAL
################################################################
## 0 - Finalitzar Programa
## 1 - Remenar Baralla
## 2 - Repartir
## 3 - Testos
## 4 - Jugar
################################################################
2
################################################################
[CC,4O,5O,AB,4C,RC,AO,SO,RE,3O,9B,RB,AC,9C,3E,7O,AE,4E,6C,5B,9E,8O,6O,9O,5C,2O,4B,8C,SC,5E,2B,8E,2C,RO,SE,2E,CB,CE,6B,3B,8B,7E,3C,7B,CO,6E,SB,7C]
## Entra un jugador de 0 a 3 que serà el que rebrà la primera.
0
[[CC,4O,5O,AB,AE,4E,6C,5B,2C,RO,SE,2E],[4C,RC,AO,SO,9E,8O,6O,9O,CB,CE,6B,3B],[RE,3O,9B,RB,5C,2O,4B,8C,8B,7E,3C,7B],[AC,9C,3E,7O,SC,5E,2B,8E,CO,6E,SB,7C]]
################################################################
```

### 3.2. Funcionament del programa de Test

El programa de test prova només la funció de trampa ja que és la més important, les altres funcions obligatòries es poden provar utilitzant les diferents constants que es designen al programa i fent ús de l'apartat 4. Exemples d'execució.

El progama de Test consta de 6 possibles opcions, 3 que retornen Nothing (que no hi ha trampa) i 3 que sí que hi ha trampa amb 3 casos de trampa diferents i explicats quan es realitza l'execució del mateix.

Exemple d'execució del programa de prova :

```haskell
################################################################
## MENÚ PROGRAMA PRINCIPAL
################################################################
## 0 - Finalitzar Programa
## 1 - Remenar Baralla
## 2 - Repartir
## 3 - Testos
## 4 - Jugar
################################################################
3
################################################################
## MENÚ TESTING DE SI HI HA TRAMPES
################################################################
## 0 - Sortir del menu Trampa
## 1 - No hi ha error test1
## 2 - No hi ha error test2 pal Butifarra
## 3 - No hi ha error capot
## 4 - Error Falla de gallines
## 5 - Error Refalla de gallines
## 6 - Error S'escapen
## 7 - Error No Mata
################################################################
1
################################################################
## Test 1
################################################################
## reparteix : 2
## Surt : 3
## Fa trumfus : 2
## Trumfus : O
################################################################
## MANS :
################################################################
## Ma del jugador 1 -> [CE,RO,4E,7C,AE,9E,9B,SO,8C,AC,5O,SB]
## Ma del jugador 2 -> [2C,4C,9O,CO,RC,5E,8E,2O,AO,SC,6B,4O]
## Ma del jugador 3 -> [8B,7E,7O,3C,SE,3B,7B,8O,5C,AB,2B,RB]
## Ma del jugador 4 -> [6O,3E,3O,9C,CC,RE,CB,2E,6E,4B,6C,5B]
################################################################
## PARTIDA :
################################################################
##  BASA 0 -> [8B,CB,9B,6B]
##  BASA 1 -> [SB,2O,2B,4B]
##  BASA 2 -> [9O,7O,3O,5O]
##  BASA 3 -> [AO,8O,6O,SO]
##  BASA 4 -> [4O,3C,2E,RO]
##  BASA 5 -> [9E,5E,7E,3E]
##  BASA 6 -> [AE,8E,SE,6E]
##  BASA 7 -> [8C,RC,5C,CC]
##  BASA 8 -> [SC,3B,9C,7C]
##  BASA 9 -> [RE,4E,2C,7B]
##  BASA 10 -> [5B,CE,4C,AB]
##  BASA 11 -> [RB,6C,AC,CO]
################################################################
## Partida Correcte!
## Punts Equip 1 -> 31
## Punts Equip 2 -> 41
## La partida acaba sense trampa, l'equip 1 format per els
## jugadors J0 i J2 perden la partida.
################################################################
```

## 4. Exemples d'execució

Els exemples d'execució que es proposen son els que té el programa de test programats. També es proposen una série d'execucions de certes funcions que s'utilitzen entre elles, concretament la funció trampa i puntsparelles o si es desitja només la funció punts parelles que ja crida a punts i trampa.

### 4.1 Pantallassos

**Escenari 1 :**

- Reparteix = 1
- Surt = 2
- Fa trunfus = 1
- Trunfus = Oros
- partida = test1
- mans = testMans
- Equip J0 J2 punts = 31
- Equip J1 J3 punts = 41

```haskell
*Main> puntsParelles testMans (Pal Oros) test1 2
Just (31,41)
*Main> trampa testMans (Pal Oros) test1 2
Nothing
```
**Escenari 2**

- Reparteix = 0
- Surt = 1
- Fa trunfus = 0
- Trunfus = Butifarra
- mans = testMans
- partida = test2
- Equip J0 J2 punts = 44
- Equip J1 J3 punts = 28

```haskell
*Main> puntsParelles testMans Butifarra test2 1
Just (44,28)
*Main> trampa testMans Butifarra test2 1
Nothing
```

**Escenari 3**

- Reparteix = 2
- Surt = 3
- Fa trunfus = 2
- Trunfus = Bastos
- mans = testMans
- partida = capot
- Equip J0 J2 punts = 72
- Equip J1 J3 punts = 0

```haskell
*Main> puntsParelles testMans (Pal Bastos) capot 3
Just (72,0)
*Main> trampa testMans (Pal Bastos) capot 3
Nothing
```

### 4.2 Exemple de partida

A continuació es mostrarà un exemple de partida per veure com es pot realitzar una partida a la butifarra amb la opció 4 del programa principal. És un exemple d'execució complert on es veu clarament com es pot realitzar una partida sencera sense fer trampes i on a la segona partida es fan trampes per part meva i acaba en un renúncio i 36 punts per els altres.

```haskell
*Main> main
################################################################
## MENÚ PROGRAMA PRINCIPAL
################################################################
## 0 - Finalitzar Programa
## 1 - Remenar Baralla
## 2 - Repartir
## 3 - Testos
## 4 - Jugar
################################################################
4
################################################################
## Comencem partida
################################################################
## S'ha fet trumfus : O
## Entra un 1 si vols que valgui per 2
-------------[3C,6B,AE,9C,SC,RB,4E,9E,5E,SB,7B,SO]--------------
0
## S'aplicarà el multiplicador : 1
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 2             JUGADOR 2                           #
#                             AO                               #
# JUGADOR 3           9O                             JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
-------------[3C,6B,AE,9C,SC,RB,4E,9E,5E,SB,7B,SO]--------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
11
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 1             JUGADOR 2                           #
#                             AO                               #
# JUGADOR 3           9O               RO            JUGADOR 1 #
#                             SO                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 3             JUGADOR 2                           #
#                                                              #
# JUGADOR 3           9B                             JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
---------------[3C,6B,AE,9C,SC,RB,4E,9E,5E,SB,7B]---------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
1
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 2             JUGADOR 2                           #
#                             5B                               #
# JUGADOR 3           9B               AB            JUGADOR 1 #
#                             6B                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 3             JUGADOR 2                           #
#                                                              #
# JUGADOR 3           CC                             JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
----------------[3C,AE,9C,SC,RB,4E,9E,5E,SB,7B]-----------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
2
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 2             JUGADOR 2                           #
#                             AC                               #
# JUGADOR 3           CC               7C            JUGADOR 1 #
#                             9C                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 0             JUGADOR 2                           #
#                                                              #
# JUGADOR 3                                          JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
------------------[3C,AE,SC,RB,4E,9E,5E,SB,7B]------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
5
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 3             JUGADOR 2                           #
#                             6E                               #
# JUGADOR 3           7E               RE            JUGADOR 1 #
#                             9E                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 0             JUGADOR 2                           #
#                                                              #
# JUGADOR 3                                          JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
-------------------[3C,AE,SC,RB,4E,5E,SB,7B]--------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
1
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 3             JUGADOR 2                           #
#                             2E                               #
# JUGADOR 3           3E               CE            JUGADOR 1 #
#                             AE                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 0             JUGADOR 2                           #
#                                                              #
# JUGADOR 3                                          JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
---------------------[3C,SC,RB,4E,5E,SB,7B]---------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
2
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 3             JUGADOR 2                           #
#                             2B                               #
# JUGADOR 3           8B               CB            JUGADOR 1 #
#                             RB                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 0             JUGADOR 2                           #
#                                                              #
# JUGADOR 3                                          JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
----------------------[3C,SC,4E,5E,SB,7B]-----------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
1
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 3             JUGADOR 2                           #
#                             RC                               #
# JUGADOR 3           6C               CO            JUGADOR 1 #
#                             SC                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 1             JUGADOR 2                           #
#                             5O                               #
# JUGADOR 3           8O               SE            JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
------------------------[3C,4E,5E,SB,7B]------------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
1
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 0             JUGADOR 2                           #
#                             5O                               #
# JUGADOR 3           8O               SE            JUGADOR 1 #
#                             4E                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 3             JUGADOR 2                           #
#                                                              #
# JUGADOR 3           6O                             JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
-------------------------[3C,5E,SB,7B]--------------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
0
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 2             JUGADOR 2                           #
#                             4O                               #
# JUGADOR 3           6O               7O            JUGADOR 1 #
#                             3C                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 1             JUGADOR 2                           #
#                             2O                               #
# JUGADOR 3           5C               8E            JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
---------------------------[5E,SB,7B]---------------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
0
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 0             JUGADOR 2                           #
#                             2O                               #
# JUGADOR 3           5C               8E            JUGADOR 1 #
#                             5E                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 2             JUGADOR 2                           #
#                             8C                               #
# JUGADOR 3           4C                             JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
----------------------------[SB,7B]-----------------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
1
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 1             JUGADOR 2                           #
#                             8C                               #
# JUGADOR 3           4C               3O            JUGADOR 1 #
#                             7B                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 1             JUGADOR 2                           #
#                             2C                               #
# JUGADOR 3           4B               3B            JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
------------------------------[SB]------------------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
0
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 0             JUGADOR 2                           #
#                             2C                               #
# JUGADOR 3           4B               3B            JUGADOR 1 #
#                             SB                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## PUNTS TOTALS :
##  BASA 0 -> [AO,9O,SO,RO]
##  BASA 1 -> [9B,6B,AB,5B]
##  BASA 2 -> [CC,9C,7C,AC]
##  BASA 3 -> [9E,RE,6E,7E]
##  BASA 4 -> [AE,CE,2E,3E]
##  BASA 5 -> [RB,CB,2B,8B]
##  BASA 6 -> [SC,CO,RC,6C]
##  BASA 7 -> [SE,5O,8O,4E]
##  BASA 8 -> [6O,3C,7O,4O]
##  BASA 9 -> [8E,2O,5C,5E]
##  BASA 10 -> [8C,4C,7B,3O]
##  BASA 11 -> [3B,2C,4B,SB]
## EQUIP 1 : 0
## EQUIP 2 : 1
################################################################


################################################################
## Comencem partida
################################################################
## S'ha fet trumfus : O
## Entra un 1 si vols que valgui per 2
-------------[9O,3E,6O,CC,5C,4C,4B,8O,9B,7E,6C,8B]--------------
1
## S'aplicarà el multiplicador : 8
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 3             JUGADOR 2                           #
#                                                              #
# JUGADOR 3           AO                             JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
-------------[9O,3E,6O,CC,5C,4C,4B,8O,9B,7E,6C,8B]--------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
0
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 2             JUGADOR 2                           #
#                             RO                               #
# JUGADOR 3           AO               SO            JUGADOR 1 #
#                             9O                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 0             JUGADOR 2                           #
#                                                              #
# JUGADOR 3                                          JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
---------------[3E,6O,CC,5C,4C,4B,8O,9B,7E,6C,8B]---------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
0
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 3             JUGADOR 2                           #
#                             RE                               #
# JUGADOR 3           6E               9E            JUGADOR 1 #
#                             3E                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 1             JUGADOR 2                           #
#                             7C                               #
# JUGADOR 3           AC               9C            JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
----------------[6O,CC,5C,4C,4B,8O,9B,7E,6C,8B]-----------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
0
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 0             JUGADOR 2                           #
#                             7C                               #
# JUGADOR 3           AC               9C            JUGADOR 1 #
#                             6O                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 0             JUGADOR 2                           #
#                                                              #
# JUGADOR 3                                          JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
------------------[CC,5C,4C,4B,8O,9B,7E,6C,8B]------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
0
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 3             JUGADOR 2                           #
#                             AB                               #
# JUGADOR 3           RC               SC            JUGADOR 1 #
#                             CC                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 3             JUGADOR 2                           #
#                                                              #
# JUGADOR 3           8C                             JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
-------------------[5C,4C,4B,8O,9B,7E,6C,8B]--------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
0
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 2             JUGADOR 2                           #
#                             CO                               #
# JUGADOR 3           8C               3C            JUGADOR 1 #
#                             5C                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 2             JUGADOR 2                           #
#                             CE                               #
# JUGADOR 3           2E                             JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
---------------------[4C,4B,8O,9B,7E,6C,8B]---------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
0
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 1             JUGADOR 2                           #
#                             CE                               #
# JUGADOR 3           2E               AE            JUGADOR 1 #
#                             4C                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 1             JUGADOR 2                           #
#                             CB                               #
# JUGADOR 3           5B               RB            JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
----------------------[4B,8O,9B,7E,6C,8B]-----------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
0
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 0             JUGADOR 2                           #
#                             CB                               #
# JUGADOR 3           5B               RB            JUGADOR 1 #
#                             4B                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 1             JUGADOR 2                           #
#                             3B                               #
# JUGADOR 3           2B               SB            JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
------------------------[8O,9B,7E,6C,8B]------------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
0
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 0             JUGADOR 2                           #
#                             3B                               #
# JUGADOR 3           2B               SB            JUGADOR 1 #
#                             8O                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 0             JUGADOR 2                           #
#                                                              #
# JUGADOR 3                                          JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
-------------------------[9B,7E,6C,8B]--------------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
0
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 3             JUGADOR 2                           #
#                             SE                               #
# JUGADOR 3           5O               7B            JUGADOR 1 #
#                             9B                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 3             JUGADOR 2                           #
#                                                              #
# JUGADOR 3           4O                             JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
---------------------------[7E,6C,8B]---------------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
0
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 2             JUGADOR 2                           #
#                             7O                               #
# JUGADOR 3           4O               6B            JUGADOR 1 #
#                             7E                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 2             JUGADOR 2                           #
#                             8E                               #
# JUGADOR 3           2O                             JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
----------------------------[6C,8B]-----------------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
0
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 1             JUGADOR 2                           #
#                             8E                               #
# JUGADOR 3           2O               5E            JUGADOR 1 #
#                             6C                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
################################################################
## Et toca tirar :
################################################################
# QUI SURT : 3             JUGADOR 2                           #
#                                                              #
# JUGADOR 3           2C                             JUGADOR 1 #
#                                                              #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
------------------------------[8B]------------------------------
## Entra de 0 a n on n és menor al nombre de cartes de la ma:
0
################################################################
## La basa ha quedat :
################################################################
# QUI SURT : 2             JUGADOR 2                           #
#                             3O                               #
# JUGADOR 3           2C               4E            JUGADOR 1 #
#                             8B                               #
#                          JUGADOR 0                           #
#                                                   TRUMFU : O #
################################################################
---------------------------RENÚNCIO!!---------------------------
## S'ha trobat trampa a la basa "[9C,7C,AC,6O]" amb numero 3
## El jugador que ha fet la trampa és : 0
################################################################
## PUNTS TOTALS :
##  BASA 0 -> [AO,9O,SO,RO]
##  BASA 1 -> [3E,9E,RE,6E]
##  BASA 2 -> [9C,7C,AC,6O]
##  BASA 3 -> [CC,SC,AB,RC]
##  BASA 4 -> [8C,5C,3C,CO]
##  BASA 5 -> [CE,2E,4C,AE]
##  BASA 6 -> [RB,CB,5B,4B]
##  BASA 7 -> [SB,3B,2B,8O]
##  BASA 8 -> [9B,7B,SE,5O]
##  BASA 9 -> [4O,7E,6B,7O]
##  BASA 10 -> [8E,2O,6C,5E]
##  BASA 11 -> [2C,8B,4E,3O]
## EQUIP 1 : 36
## EQUIP 2 : 1
################################################################
```

## 5. Conclusions

La pràctica és molt divertida de fer i és molt entretinguda. Hem arrivat a la conclusió que el haskell és una canya, que el que realment penses i plasmes és el que fa i que no t'emportes gaires sorpreses.

La col·laboració entre nosaltres ha sigut constant, la pràctica l'hem fet els dos junts amb puntuals actuacions en solitari fruit d'una il·lumunada o d'un brot d'il·lusió per programar la butifarra.

La documentació està feta en Markdown i generada amb pandoc (utilitza latex) i es fa servir la plantilla [einvogel](https://github.com/Wandmalfarbe/pandoc-latex-template).

Per a la implementació de la pràctica i com a repositori s'ha fet servir git allotjat a [bitbucket](http://bitbucket.org).
