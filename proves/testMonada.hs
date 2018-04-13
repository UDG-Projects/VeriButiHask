import System.Random

ronda:: Int -> [Int] -> [Int]
ronda primer llista = if (length llista) < 4 then  (ronda jugador novaLlista) else llista
 where
   jugador = seguentJugador primer
   novaLlista =llista ++ [jugador]


seguentJugador :: Int -> Int
seguentJugador jugador = mod (jugador + 1) 4

jugar :: [Int] -> [Int] -> [Int] -> Int -> IO ()
jugar [] _ _ _ = do putStrLn("Fi partida")
jugar randoms llistaJugadors partida quiTira = do

  --let num = take 1 (mans!!quiTira)
  let quiSoc = 1
  let quiTirara = (seguentJugador quiTira)
  if quiTira == quiSoc then do
    putStrLn("Entra numero")
    opcio <- getLine
    let numOp = (read opcio :: Int)
  --  partida++[numOP]
    putStrLn ("Jo Tiro: " ++ (show numOp))
--    jugar randoms llistaJugadors (partida++[num]) (seguentJugador quiComenca)
  else do
    putStrLn("El jugador " ++ (show quiTira))

  if (llistaJugadors!!3) == quiTira then do

    let guanyador = head randoms
    putStrLn("Guanyador: "++(show guanyador))
    jugar (drop 1 randoms) (ronda guanyador [guanyador]) partida guanyador
  else do
    jugar randoms llistaJugadors partida quiTirara

  putStrLn("")
--  if (mod (length partida) 4 ) == 0 && (length partida) /= 0 then do
--    let basa = last 4 partida
--    let posicioJugador = snd( maximum ( zip basa llistaJugadors))
  --  let jugadors = posicioJugador: [ x | x <- [1..3]]
--  let novaMans = take quiComenca mans ++ (drop 1 (mans!!quiComenca)) ++ (drop (quiComenca + 1) mans)
--  jugar mans llistaJugadors (partida++num) quiTirara

test:: [Int] -> IO ( [Int] )
test [] = do
  return ([1])
test xs = do
  return ( 2:xs)

main = do
  seed <- newStdGen
  let random = take 12 (randomRs (0 :: Int,3) seed)
  jugar random (ronda 2 [2]) [] 2 -- Comencça el jugador 3
