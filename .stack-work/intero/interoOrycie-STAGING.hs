module jackLambda () where

import System.Random 
import DataTypes 


main :: IO ()
main = do
  return ()



menuCarga :: IO String -> IO ()
menuCarga = do
  cargar <-  putStrLn "Desea Cargar una partida? (y/n) " >> getLine
  case cargar of
    "y" -> do
      partida <- putStrLn "Indique cual partida desea reanudar" >> getLine
    "n" -> crearPartida
     _  -> putStrLn "Indique una opcion valida" >> menuCarga  
  return ()

crearPartida :: IO ()
crearPartida = do
  nombre <- getNombre
  dineroInicial <- getDinero
  dineroGanar <- getDineroGanar dineroInicial
  dineroRonda <- getDineroRonda dineroInicial
  juegoNuevo nombre dineroInicial dineroGanar dineroRonda
  return ()

juegoNuevo :: IO String -> IO Int -> IO Int -> IO Int -> IO ()
juegoNuevo nombre dinero objetivo apuesta = do
  juegoActual <- GS { 
                    juegosJugados = 0,
                    victoriasLambda = 0,
                    nombre = nombre,
                    generador = getStdGen,
                    dinero = dinero,
                    objetivo = objetivo,
                    apuesta = apuesta
                    }
  menuJuego juegoActual
  return ()

menuJuego :: GameState -> IO ()
menuJuego datos = do
  putStrLn "Jugar Ronda (1)"
  putStrLn "Guardar Partida (2)"
  putStrLn "Cargar Partida (3)"
  opcion <- putStr "Opcion: " >> getLine
  case opcion of
    "1" -> jugarRonda datos
    "2" -> guardarPartida datos
    "3" -> cargarPartida datos
    _   -> putStrLn "Escoja una opcion valida" >> menuJuego datos
  return ()

jugarRonda :: GameState -> IO ()
jugarRonda datos = do
  -- Dinero de la ronda
  let dinero = ((-) <$> dinero <*> apuesta) datos
  -- Nuevo estado con dinero:
  let datos' = datos {dinero = dinero}
  -- Se baraja el mazo inicial
  let m0 = barajar (generador datos) baraja
  -- Se le dan 2 cartas a jack, y se les descuenta del mazo
  let (manoLambda,m1) = inicialLambda m0
  -- Verificacion de blackJack por parte de jack
  if blackJack manoLambda then ganaLambdaBlackJack datos' else ()
  -- Se inicializa la mano del jugadir
  let (manoPlayerInicial,m2) = desdeMano <$> (inicialPlayer m1)
  putStrLn $ nombre datos' ++ "Esta es mi primera carta" ++ show manoLambda
  -- Seleccion de mazo.
  putStrLn  $ nombre datos' ++ " robaras de la Izquierda o de la Derecha (Izquierdo/Derecho)"
  choice <- (read::String -> Eleccion) . getLine
  -- Completamos la mano del jugador.
  -- Notemos que dado que esta es la mano inicial, NUNCA vamos a obtener un
  -- non exhaustive pattern.
  let Just (mazo,manoPlayer) = robar m2 manoPlayerInicial choice
  -- Print de la mano inicial del jugador. Y verificacion de blackjack
  if blackJack manoPlayer then ganaPlayerBlackJack datos' else showMano manoPlayer
  menuRonda datos' mazo manoPlayer manoLambda 
  return ()
  
menuRonda :: GameState -> IO ()
menuRonda datos mazo manoPlayer manoLambda  = do
  let puedeDouble = (<) 0 $ ((-) <$> dinero <*> apuesta) datos
  let puedeSurrender = cantidadCartas manoPlayer == 2
  putStrLn "Seleccione una opcion:"
  putStrLn "Hit (h)"
  putStrLn "Stand (s)"
  if puedeDouble then putStrLn "Double Down (d)" else ()
  if puedeSurrender then putStrLn "Surrender (ss)" else ()
  opcion <- putStr "Opcion: " >> getLine
  case (opcion, puedeDouble, puedeSurrender) of
    ("h", _, _)  -> hit datos mazo manoPlayer manoLambda 
    ("s",_,_)    -> stand datos mazo manoPlayer manoPlayer 
    ("d",True,_) -> doubleDown datos 
    ("ss",_,_)   -> surrender datos
    (_,_,_)      -> putStrLn "Indique una opcion valida" >> menuRonda datos mazo manoPlayer manoLambda
  
  return ()
  
hit :: GameState -> Mazo -> Mano -> Mano -> IO ()
hit datos mazo manoPlayer manoLambda = do
  let dividir = puedePicar mazo
  let mazo' = if dividir then mazo else (reconstruir . (reconstruir mazo)) manoPlayer manoLambda
  eleccion <- (putStr $ nombre datos ++ "robaras de izquierda o de derecha? (Izquierdo/Derecho)") >>  (read::String -> Eleccion) . getLine
  

data GameState = GS {
  juegosJugados   :: Int,
  victoriasLambda :: Int,
  nombre          :: String,
  generador       :: StdGen,
  dinero          :: Int,
  objetivo        :: Int,
  apuesta         :: Int
  }
