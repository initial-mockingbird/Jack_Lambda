module JackLambda (main) where

import System.Random 
import DataTypes 
import Control.Monad

getInt :: IO Int
getInt = do
  entero <- getLine
  return ( (read entero) :: Int )
  
main :: IO ()
main = menuCarga

menuCarga :: IO ()
menuCarga = do
  cargar <-  putStr "Desea Cargar una partida? (y/n): " >> getLine
  case cargar of
    "y" -> do
      partida <- putStrLn "Indique cual partida desea reanudar" >> getLine
      return ()
    "n" -> crearPartida
    (_) -> putStrLn "Indique una opcion valida" >> menuCarga  
  return ()

crearPartida :: IO ()
crearPartida = do
  nombre <- getNombre
  dineroInicial <- getDinero
  dineroGanar <- getDineroGanar dineroInicial
  dineroRonda <- getDineroRonda dineroInicial
  juegoNuevo nombre dineroInicial dineroGanar dineroRonda
  return ()

getNombre :: IO String
getNombre = do
  nombre <- putStr "Indique su nombre: " >> getLine 
  return nombre
  
getDinero :: IO Int
getDinero = do
  dinero <- putStr "Indique la cantidad de dinero con la que se debe empezar: " >> getInt
  return dinero

getDineroGanar :: Int -> IO Int
getDineroGanar dineroInicial = do
  dineroGanar <- putStr "Indique la cantidad de dinero con la que se gana: " >> getInt
  if dineroGanar <= dineroInicial then putStrLn "La cantidad para ganar debe ser estrictamente mayor que la que se tiene, intente nuevamente" >>
    getDineroGanar dineroInicial else return dineroGanar 

getDineroRonda :: Int -> IO Int
getDineroRonda dineroInicial = do
  dineroRonda <- putStr "Indique la cantidad de dinero por apuesta: " >> getInt
  if  dineroRonda < dineroInicial then putStrLn "La apuesta  debe ser mayor o igual que la que se tiene, intente nuevamente" >>
    getDineroRonda dineroInicial else return dineroRonda
  
juegoNuevo :: String -> Int -> Int -> Int -> IO ()
juegoNuevo nombre dinero objetivo apuesta = do
  gen <- getStdGen 
  let juegoActual = GS { 
                    juegosJugados = 0,
                    victoriasLambda = 0,
                    nombre = nombre,
                    generador = gen,
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
  putStrLn $ "Modificar apuesta: <apuesta actual: " ++ ((show . apuesta) datos) ++"> (4)"
  opcion <- putStr "Opcion: " >> getLine
  case opcion of
    "1" -> jugarRonda datos
    "2" -> guardarPartida datos
    "3" -> cargarPartida datos
    "4" -> modificarApuesta datos 
    _   -> putStrLn "Escoja una opcion valida" >> menuJuego datos
  return ()

guardarPartida :: GameState -> IO ()
guardarPartida = undefined

cargarPartida :: GameState -> IO ()
cargarPartida = undefined

modificarApuesta :: GameState -> IO ()
modificarApuesta = undefined 

jugarRonda :: GameState -> IO ()
jugarRonda datos = do
  -- Dinero de la ronda
  let dinero' = (dinero datos) - (apuesta datos)
  -- Nuevo estado con dinero:
  let datos' = datos {dinero = dinero'}
  -- Se baraja el mazo inicial
  let m0 = barajar (generador datos) baraja
  -- Se le dan 2 cartas a jack, y se les descuenta del mazo
  let (manoLambda,m1) = inicialLambda m0
  -- Verificacion de blackJack por parte de jack
  if blackJack manoLambda then ganaLambdaBlackJack datos' else pure ()
  -- Se inicializa la mano del jugadir
  let (manoPlayerInicial,m2) = desdeMano <$> (inicialPlayer m1)
  putStrLn $ nombre datos' ++ "Esta es mi primera carta: " ++ show manoLambda
  -- Seleccion de mazo.
  putStr  $ nombre datos' ++ " robaras de la Izquierda o de la Derecha (Izquierdo/Derecho): "
  choice <-  getLine
  -- Completamos la mano del jugador.
  -- Notemos que dado que esta es la mano inicial, NUNCA vamos a obtener un
  -- non exhaustive pattern.
  let Just (mazo,manoPlayer) = robar m2 manoPlayerInicial ((read choice) :: Eleccion)
  -- Print de la mano inicial del jugador. Y verificacion de blackjack
  if blackJack manoPlayer then ganaPlayerBlackJack datos' else putStrLn (showMano manoPlayer)
  menuRonda datos' mazo manoPlayer manoLambda 
  return ()
  
menuRonda :: GameState -> Mazo -> Mano -> Mano -> IO ()
menuRonda datos mazo manoPlayer manoLambda  = do
  let puedeDouble = (<) 0 $ ((-) <$> dinero <*> apuesta) datos
  let puedeSurrender = cantidadCartas manoPlayer == 2
  putStrLn "Seleccione una opcion:"
  putStrLn "Hit (h)"
  putStrLn "Stand (s)"
  if puedeDouble then putStrLn "Double Down (d)" else pure ()
  if puedeSurrender then putStrLn "Surrender (ss)" else pure () 
  opcion <- putStr "Opcion: " >> getLine
  case (opcion, puedeDouble, puedeSurrender) of
    ("h", _, _)  -> hit datos mazo manoPlayer manoLambda 
    ("s",_,_)    -> stand datos mazo manoPlayer manoLambda 
    ("d",True,_) -> doubleDown datos mazo manoPlayer  manoLambda 
    ("ss",_,_)   -> surrender datos
    (_,_,_)      -> putStrLn "Indique una opcion valida" >> menuRonda datos mazo manoPlayer manoLambda
  
  return ()
  
hit :: GameState -> Mazo -> Mano -> Mano -> IO ()
hit datos mazo manoPlayer manoLambda = do
  let dividir = puedePicar mazo
  -- Si no se puede dividir el mazo, entonces se reconstruye con las cartas faltantes.
  let mazo' = if dividir then mazo else (reconstruir . (reconstruir mazo)) manoPlayer manoLambda
  eleccion <- (putStr $ nombre datos ++ "robaras de izquierda o de derecha? (Izquierdo/Derecho)") >>  getLine
  -- Notemos que si el mazo se puede dividir entonces hay al menos 1 carta en el mazo izquierdo o derecho
  -- de manera similar, al reconstruir el mazo, siempre se va a poder dividir en 2 mitades
  -- por lo tanto, podemos hacer el siguiente pattern matching sin obtener non-exhaustive pattern exception.
  let Just (mazo'',manoPlayer') = robar mazo' manoPlayer ((read eleccion) :: Eleccion )
  if valor manoPlayer' > 21 then ganaLambda datos manoPlayer' manoLambda else putStrLn (showMano manoPlayer') >>
    menuRonda datos mazo'' manoPlayer' manoLambda

  return ()

stand ::  GameState -> Mazo -> Mano -> Mano -> IO ()
stand datos mazo manoPlayer manoLambda = do
  putStrLn "Es mi turno ahora." >> turnoLambda datos mazo manoPlayer manoLambda

doubleDown ::  GameState -> Mazo -> Mano -> Mano -> IO ()
doubleDown datos mazo manoPlayer manoLambda = do
  let datos' = datos {dinero = ((-) <$> dinero <*> apuesta) datos, apuesta = (*2) $ apuesta datos }
  (mazo', manoPlayer') <-  drawingPhasePlayer datos mazo manoPlayer manoLambda
  if valor manoPlayer' > 21 then ganaLambda datos manoPlayer' manoLambda else (putStrLn . showMano) manoPlayer' >> stand datos' mazo' manoPlayer' manoLambda
  return ()


surrender ::  GameState -> IO ()
surrender datos = do
  let datos' = datos {dinero = dinero datos + (apuesta datos `div` 2) }
  if dinero datos >= apuesta datos then menuJuego datos' else putStrLn $ nombre datos' ++  " no te queda dinero. Este es el fin para ti."
  return ()    

drawingPhasePlayer ::  GameState -> Mazo -> Mano -> Mano -> IO (Mazo,Mano)
drawingPhasePlayer datos mazo manoPlayer manoLambda = do
  let dividir = puedePicar mazo
  -- Si no se puede dividir el mazo, entonces se reconstruye con las cartas faltantes.
  let mazo' = if dividir then mazo else (reconstruir . (reconstruir mazo)) manoPlayer manoLambda
  eleccion  <- (putStr $ nombre datos ++ "robaras de izquierda o de derecha? (Izquierdo/Derecho)") >>  getLine
  -- Notemos que si el mazo se puede dividir entonces hay al menos 1 carta en el mazo izquierdo o derecho
  -- de manera similar, al reconstruir el mazo, siempre se va a poder dividir en 2 mitades
  -- por lo tanto, podemos hacer el siguiente pattern matching sin obtener non-exhaustive pattern exception.
  let Just (mazo'',manoPlayer') = robar mazo' manoPlayer ( (read eleccion) :: Eleccion )
  return (mazo'',manoPlayer')


turnoLambda :: GameState -> Mazo -> Mano -> Mano -> IO ()
turnoLambda datos mazo manoPlayer manoLambda = do
  -- Nunca va a dar non exhaustive pattern debido a que cada vez que se roba se reconstruye.
  let f (Just (mazo',mano')) = if valor mano' < 16 then f (robar (reconstruir mazo' (manoPlayer `appendMano` mano') ) mano' Izquierdo)
        else Just (mazo',mano') 
  let Just (_,manoLambda') = f $ Just (mazo,manoLambda)
  let winner = ganador manoLambda' manoPlayer 
  case winner of
    Player -> ganaPlayer datos manoPlayer manoLambda'
    Dealer -> ganaLambda datos manoPlayer manoLambda' 


ganaLambdaBlackJack :: GameState -> IO ()
ganaLambdaBlackJack = undefined

ganaPlayerBlackJack :: GameState -> IO ()
ganaPlayerBlackJack = undefined

showMano :: Mano -> String
showMano = undefined 


ganaLambda :: GameState -> Mano -> Mano -> IO ()
ganaLambda = undefined 

ganaPlayer :: GameState -> Mano -> Mano -> IO ()
ganaPlayer = undefined

data GameState = GS {
  juegosJugados   :: Int,
  victoriasLambda :: Int,
  nombre          :: String,
  generador       :: StdGen,
  dinero          :: Int,
  objetivo        :: Int,
  apuesta         :: Int
  } deriving(Eq,Show)
