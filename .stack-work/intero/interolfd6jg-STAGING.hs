module DataTypes (
  Palo,
  Rango,
  Carta,
  Jugador,
  Mano,
  vacia,
  baraja ) where 
 

import System.Random
--------------------------
--- Tipos de Dato: Carta
--------------------------
data Palo = Treboles | Diamantes | Picas | Corazones deriving(Eq,Enum,Bounded)

data Rango = N Int | Jack | Queen | King | Ace  deriving(Eq)


data Carta = Carta {
  rango :: Rango,
  palo  :: Palo
                   } deriving(Eq)
--------------------
-- Show instances
-------------------
  
instance Show Palo where
  show Treboles = "\2663"
  show Diamantes = "\2666"
  show Picas = "\2660"
  show Corazones = "\2665"

instance Show Rango where
  show ( Jack) = "J"
  show ( Queen) = "Q"
  show ( King) = "K"
  show ( Ace) = "A"
  show (N n) = show n 

instance Show Carta where
  show  = (++) <$> (show . palo) <*> (show . rango) where

  
----------------------
-- Bounded Instances
----------------------

instance Bounded Rango where
  minBound = Ace
  maxBound = King

instance Bounded Carta where
  minBound = Carta {rango = minBound,palo=minBound}
  maxBound = Carta {rango = maxBound,palo=maxBound}
-------------------
-- Enum Instances
-------------------

instance Enum Rango where
  succ (N n) | n < 10 = N $ succ(n)
             | otherwise = Jack
  succ Jack = Queen
  succ Queen = King
  succ King = error "succ King out of bounds"
  succ Ace = N 2
  
  pred (N n) | n > 2 =  N $ pred(n)
             | otherwise = Ace
  pred Jack = N 10
  pred Queen = Jack
  pred King = Queen
  pred Ace = error "pred Ace out of bounds"
  toEnum x | 2<= x && x <= 10 = N x
           | x == 1 = Ace
           | x == 11 = Jack
           | x == 12 = Queen
           | x == 13 = King
           | otherwise = error "Out of bounds"
  fromEnum (N n) = n
  fromEnum Ace = 1
  fromEnum Jack = 11
  fromEnum Queen = 12
  fromEnum King = 13
  enumFrom     x   = enumFromTo     x maxBound
  enumFromThen x y = enumFromThenTo x y bound
      where
        bound | fromEnum y >= fromEnum x = maxBound
              | otherwise                = minBound
              
instance Enum Carta where
  succ (Carta valor pica) | valor == maxBound = Carta {rango = minBound, palo = succ(pica)}
                          | otherwise = Carta {rango = succ(valor),palo = pica}
                          
  pred (Carta valor pica) | valor == minBound  = Carta {rango = maxBound, palo = pred(pica)}
                          | otherwise = Carta{rango = pred(valor),palo = pica}
                         
  toEnum x | 1<= x && x<= 52 = Carta {rango = toEnum $ (x-1) `mod` 13 + 1,palo = toEnum $ abs (x-1) `div` 13}
           | otherwise = error "Out of Bound"
           
  fromEnum (Carta valor pica) = (fromEnum valor) + 13 * fromEnum pica
  
  enumFrom     x   = enumFromTo     x maxBound
  
  enumFromThen x y = enumFromThenTo x y bound
      where
        bound | fromEnum y >= fromEnum x = maxBound
              | otherwise                = minBound
--- Tipos de Dato: Jugador.

data Jugador = Dealer | Player

--- Tipo de Datos: Mano.

newtype Mano = Mano [Carta] deriving (Eq,Show)

-- Funciones de Mano requeridas:

vacia :: Mano
vacia = Mano ([] :: [Carta])

baraja :: Mano
baraja =  Mano [minBound::Carta ..]

cantidadCartas :: Mano-> Int
cantidadCartas (Mano xs) = length xs

valor :: Mano -> Int
valor (Mano xs) = switch $ foldr f (0,0) xs  where
  f :: Carta -> (Int,Int) -> (Int,Int)
  f (Carta valor _) (as,acc)  | fromEnum valor >=10 = (as,acc+10)
                              | valor == Ace = (as+1,acc+10)
                              | otherwise = (as,acc + fromEnum valor)

switch :: (Int,Int) -> Int
switch (as,acc) = if acc>21 then acc-as*10 + as else acc

busted :: Mano -> Bool
busted = ((<) 21 ) . valor

blackJack :: Mano -> Bool
blackJack = ((==) 21) . valor

ganador :: Mano -> Mano -> Jugador
ganador dealer player | busted dealer = Player
                      | busted player = Dealer
                      | valor dealer >= valor player = Dealer
                      | otherwise = Player

separar :: Mano -> (Mano,Carta,Mano)
separar (Mano m) = (Mano l, foldr const (minBound::Carta)  c, Mano r) where
                     n = length m
                     half = n `div` 2
                     (l,(c,r)) = (splitAt 1) <$> (splitAt half m)
