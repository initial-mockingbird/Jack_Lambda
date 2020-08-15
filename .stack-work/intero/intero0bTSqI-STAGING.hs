module DataTypes (
  Palo,
  mkN,
  Rango,
  Carta,
  Jugador,
  Mano,
  vacia,
  baraja ) where 
  
  

--- Tipos de Dato: Carta 
data Palo = Treboles | Diamantes | Picas | Corazones deriving(Eq,Enum)

newtype CartaNumerica = CartaNumerica {n :: Int} deriving(Eq,Ord)

mkN :: Int -> CartaNumerica
mkN x | 2<= x && x <= 10 = CartaNumerica x
      | otherwise = error $ show x ++ "Is not a valid card number"

data Letrica =  Jack | Queen | King | Ace  deriving (Eq,Enum)

data Rango = N CartaNumerica | L Letrica  deriving(Eq)

data Carta = Carta {
  rango :: Rango,
  palo  :: Palo
                   } deriving(Eq)

-- Show instances
instance Show CartaNumerica where
  show (CartaNumerica n) = show n
  
instance Show Palo where
  show Treboles = "\2663"
  show Diamantes = "\2666"
  show Picas = "\2660"
  show Corazones = "\2665"

instance Show Rango where
  show (L Jack) = "J"
  show (L Queen) = "Q"
  show (L King) = "K"
  show (L Ace) = "A"
  show (N n) = show n 

instance Show Carta where
  show  = (++) <$> (show . palo) <*> (show . rango) where


-- Enum Instances


instance Enum CartaNumerica where
  succ (CartaNumerica n) | 2<=n && n<10 = CartaNumerica (n+1)
                         | otherwise = error $ show n  ++ "Es muy grande para tener un sucesor"
  fromEnum (CartaNumerica n) = n
  enumFrom n | 2 <= fromEnum n && fromEnum n<10 = n : (enumFrom . succ) n
             | fromEnum n == 10 =  CartaNumerica 10 : []
             | otherwise =  error $ show n  ++ "Es muy grande"
  toEnum n | 2<=n && n<=10 = CartaNumerica (n)
                         | otherwise = error $ show n  ++ "Es muy grande"

instance Enum Rango where
  succ (N n) | fromEnum n == 10 =  L Jack
             | otherwise = N $ succ n
  succ (L l) = L (succ l)
  fromEnum (N n) = fromEnum n
  fromEnum (L l) = (11 + fromEnum l) 
  toEnum n | 2<=n && n<=10 = N $ toEnum n
           | 11<=n && n<=14  = L $ toEnum (n `mod` 11)
           | otherwise = error $ show n  ++ "Es muy grande"
  enumFrom x | x == L Ace = L Ace : []
             | otherwise = x : (enumFrom . succ) x
  
instance Enum Carta where
  succ (Carta valor pal)   | fromEnum valor == 0 && not (pal == Corazones) = Carta {rango = toEnum 2, palo = succ pal}
                           | fromEnum valor == 0 &&  (pal == Corazones) =  error $ show (Carta{rango=valor,palo=pal})  ++ "Es muy grande"
                           | otherwise =  Carta {rango = succ valor , palo = pal}
  fromEnum (Carta valor pal) = fromEnum valor + 13*(fromEnum pal)
  toEnum x = Carta {rango = toEnum $ x `mod` 14,palo = toEnum (div x 14)}
  enumFrom (Carta valor pal) = [Carta x y | x <- enumFrom valor, y <- enumFrom pal]
    
--- Tipos de Dato: Jugador.

data Jugador = Dealer | Player

--- Tipo de Datos: Mano.

newtype Mano = Mano [Carta]

-- Funciones de Mano requeridas:

vacia :: Mano
vacia = Mano ([] :: [Carta])

baraja :: Mano
baraja =  [Carta {rango = toEnum 2, palo = Treboles} ..]
