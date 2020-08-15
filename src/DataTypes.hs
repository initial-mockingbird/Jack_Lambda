module DataTypes (
  Jugador(..),
  Eleccion(..),
  Mazo,
  Mano,
  busted,
  blackJack,
  ganador,
  barajar,
  cantidadCartas,
  inicialLambda,
  robar,
  juegaLambda,
  puedePicar,
  inicialPlayer,
  desdeMano,
  valor,
  reconstruir,
  appendMano,
  baraja ) where 
 

import System.Random
--------------------------
--- Tipos de Dato
--------------------------

data Jugador = Dealer | Player deriving (Eq,Show,Read)

newtype Mano = Mano [Carta] deriving (Eq)

data Palo = Treboles | Diamantes | Picas | Corazones deriving(Eq,Enum,Bounded)

data Rango = N Int | Jack | Queen | King | Ace  deriving(Eq)

data Carta = Carta {
  rango :: Rango,
  palo  :: Palo
                   } deriving(Eq)

data Mazo = Vacio | Mitad Carta Mazo Mazo deriving(Eq,Show)

data Eleccion = Izquierdo | Derecho deriving(Eq,Show,Read)

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

instance Show Mano where
  show (Mano m) = foldr (++) "" $ show <$> m
  
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
                              | valor == Ace = (as+1,acc+11)
                              | otherwise = (as,acc + fromEnum valor)

switch :: (Int,Int) -> Int
switch (as,acc) = if acc>21 then acc-as*11 + as else acc

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
separar m  = separar' m half where
  half = cantidadCartas m `div` 2

barajar :: StdGen -> Mano -> Mano
barajar _ (Mano []) = (Mano [])
barajar g  m =
  let
    n = cantidadCartas m
    longitud = (1,n-1)
    (value,gen) = uniformR longitud g
    (l ,c, r) = separar' m value
    pending = l `appendMano` r 
  in (Mano [c]) `appendMano`  (barajar gen pending )
  
inicialLambda :: Mano -> (Mano,Mano)
inicialLambda (Mano m) = let
  (lambda,mazo) = splitAt 2 m
  in (Mano lambda,Mano mazo)

desdeMano :: Mano -> Mazo
desdeMano (Mano []) = Vacio
desdeMano m = let
  (l,c,r) = separar m
  in Mitad c (desdeMano l) (desdeMano r)

puedePicar :: Mazo -> Bool
puedePicar Vacio = False
puedePicar (Mitad _ Vacio _) = False
puedePicar (Mitad _ _ Vacio) = False
puedePicar _ = True


aplanar :: Mazo -> Mano
aplanar Vacio = vacia
aplanar (Mitad c l r) = aplanar l  `appendMano` Mano [c] `appendMano` aplanar r

reconstruir :: Mazo -> Mano -> Mazo
reconstruir mazo mano = desdeMano $  (delete . aplanar) mazo mano where
  delete :: Mano -> Mano -> Mano
  delete (Mano mazo') (Mano mano') = Mano [cartas | cartas <- mazo', cartas `notElem` mano']

robar :: Mazo -> Mano -> Eleccion -> Maybe (Mazo,Mano)
robar mazo mano eleccion = let
  mazo' = aplanar mazo
  (mazoI',c,mazoD) = separar mazo'
  mazoI = Mano [c] `appendMano` mazoI'
  manoIzquierda = if (mazoI == vacia) then Nothing else Just $ takeFromMano 1 mazoI `appendMano` mano
  manoDerecha = if (mazoD == vacia) then Nothing else Just $ takeFromMano 1 mazoD `appendMano` mano
  izquierdo = reconstruir' (desdeMano mazoI) manoIzquierda
  derecho = reconstruir' (desdeMano mazoD) manoDerecha 
  in case eleccion of
       Izquierdo -> sequenceA (izquierdo, manoIzquierda)
       Derecho  -> sequenceA (derecho, manoDerecha)


robar' :: Mazo -> Mano -> Eleccion -> Maybe (Mazo,Mano)
robar' mazo mano eleccion = let
  mitades = (inicialLambda . aplanar) mazo
  obtenerMano = \x -> if x == vacia then Nothing else Just $ takeFromMano 1 x `appendMano` mano
  manos = bimap obtenerMano obtenerMano mitades
  decision =  (bimap reconstruir' reconstruir' (bimap desdeMano desdeMano mitades)) <<*>> manos
  in case eleccion of
       Izquierdo -> sequenceA $ bimap fst fst (decision, manos)
       Derecho -> sequenceA $ bimap snd snd (decision, manos)

juegaLambda :: Mazo -> Mano -> Maybe Mano
juegaLambda mazo mano | valor mano >= 16 = Just mano
                      | aplanar mazo == vacia  = Nothing
                      | otherwise = juegaLambda mazo' mano' where
                          mano' = (takeFromMano 1 $ aplanar mazo ) `appendMano` mano
                          mazo' = reconstruir mazo mano'
---------------------
-- Helper Functions
---------------------

takeFromMano :: Int -> Mano -> Mano
takeFromMano n (Mano mano)  = Mano $ take n mano

separar' :: Mano -> Int ->  (Mano,Carta,Mano)
separar' (Mano []) _  = error "Es imposible separar manos vacias"
separar' (Mano (x:[])) _ = (vacia,x,vacia)
separar' (Mano m) separator  = (Mano l, foldr const (minBound::Carta)  c, Mano r) where
                     n = length m
                     (l,(c,r)) = (splitAt 1) <$> (splitAt separator m)
                     
appendMano :: Mano -> Mano -> Mano
appendMano (Mano xs) (Mano ys) = Mano $ xs ++ ys

reconstruir' :: Mazo -> Maybe Mano -> Mazo
reconstruir' mazo mano = case mano of
  Nothing  -> mazo
  (Just m) -> reconstruir mazo m


inicialPlayer :: Mano -> (Mano,Mano)
inicialPlayer (Mano m) = let
  (player,mazo) = splitAt 1 m
  in (Mano player,Mano mazo)


--------------------
-- Helper Instances
--------------------
class Bifunctor p where
  -- | Map over both arguments at the same time.
  --
  -- @'bimap' f g ≡ 'first' f '.' 'second' g@
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
  {-# INLINE bimap #-}

  -- | Map covariantly over the first argument.
  --
  -- @'first' f ≡ 'bimap' f 'id'@
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  {-# INLINE first #-}

  -- | Map covariantly over the second argument.
  --
  -- @'second' ≡ 'bimap' 'id'@
  second :: (b -> c) -> p a b -> p a c
  second = bimap id
  {-# INLINE second #-}

instance Bifunctor (,) where
  bimap f g ~(a, b) = (f a, g b)
  {-# INLINE bimap #-}


class Bifunctor p => Biapplicative p where
  bipure :: a -> b -> p a b

  (<<*>>) :: p (a -> b) (c -> d) -> p a c -> p b d

instance Biapplicative (,) where
  bipure = (,)
  {-# INLINE bipure #-}
  (f, g) <<*>> (a, b) = (f a, g b)
  {-# INLINE (<<*>>) #-}
