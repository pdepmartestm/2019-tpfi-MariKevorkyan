import Text.Show.Functions
import Data.List

data Pirata = UnPirata {
apodo :: String,
botin :: [Tesoro] } deriving (Show,Eq)

--data Tesoro = UnTesoro {
--nombre :: String,
--valor :: Int } deriving (Show,Eq)

jackSparrow = UnPirata "Jack" [brujula, frascoArena]
davidJones = UnPirata "Jones" [cajitaMusical]
anneBonny = UnPirata "Anne" [doblones, frascoDeArena] 

brujula = UnTesoro "brujula que apunta lo que mas deseas" 10000
frascoDeArenaConValorCero = UnTesoro "frasco de arena" 0
cajitaMusical = UnTesoro "cajita musical" 1
doblones = UnTesoro "doblones" 100
frascoDeArenaConValorUno = UnTesoro "frasco de arena" 1
oro = UnTesoro "oro" 100

----1
tesoroNoValioso :: Tesoro -> Bool
tesoroNoValioso tesoro = valor tesoro <= 100

mismoTesoroDistintoValor :: Tesoro -> Tesoro -> Bool
mismoTesoroDistintoValor tesoro1 tesoro2 = (nombre tesoro1 == nombre tesoro2) && (valor tesoro1 /= valor tesoro2)

esParteDelBotin :: [Tesoro] -> Tesoro -> Bool
esParteDelBotin botin tesoro = any (mismoTesoroDistintoValor tesoro) botin

hayMismoTesoroConOtroValor :: [Tesoro] -> [Tesoro] -> Bool
hayMismoTesoroConOtroValor unBotin otroBotin = any (esParteDelBotin unBotin) otroBotin

tienenMismoTesoroConOtroValor :: Pirata -> Pirata -> Bool
tienenMismoTesoroConOtroValor unPirata otroPirata = hayMismoTesoroConOtroValor (botin unPirata) (botin otroPirata)

--Cantidad Tesoros
cantTesoros :: Pirata -> Int
cantTesoros pirata = length (botin pirata)

valoresDeTesoros :: Pirata -> [Int]
valoresDeTesoros pirata = map valor (botin pirata)

valorBotin :: Pirata -> Int
valorBotin pirata = sum (valoresDeTesoros pirata)

--pirata es afortunado
pirataAfortunado :: Pirata -> Bool
pirataAfortunado pirata = (valorBotin pirata) > 10000

--valor del tesoro mas valioso
valorTesoroMasValioso :: Pirata -> Int
valorTesoroMasValioso pirata = maximum (valoresDeTesoros pirata)

--adquirir nuevo tesoro
adquirirTesoro :: Tesoro -> Pirata -> Pirata
adquirirTesoro tesoro pirata = pirata {botin = tesoro : (botin pirata)}

--perder tesoros

perderTesorosSegun :: FormaDeSaqueo -> Pirata -> Pirata
perderTesorosSegun cond pirata = pirata {botin = filter cond(botin pirata)}

perderValiosos :: Pirata -> Pirata
perderValiosos pirata = perderTesorosSegun tesoroNoValioso pirata

tesoroNombreDistinto :: String -> Tesoro -> Bool
tesoroNombreDistinto nombre1 tesoro = nombre tesoro /= nombre1

perderTesorosNombre :: String -> Pirata -> Pirata
perderTesorosNombre nombre pirata = perderTesorosSegun (tesoroNombreDistinto nombre) pirata

------------------------------------SAQUEOS------------------------------------------
type FormaDeSaqueo = (Tesoro -> Bool)

saqueoValioso :: Tesoro -> Bool --= :: FormaDeSaqueo
saqueoValioso tesoro = (not.tesoroNoValioso) tesoro

saqueoEspecifico :: String -> Tesoro -> Bool -- = String -> FormaDeSaqueo
saqueoEspecifico objeto tesoro = nombre tesoro == objeto

saqueoConCorazon :: Tesoro -> Bool
saqueoConCorazon tesoro = False

saqueoComplejo :: String -> Tesoro -> Bool
saqueoComplejo objeto tesoro = saqueoValioso tesoro || saqueoEspecifico objeto tesoro

saquear :: FormaDeSaqueo -> Tesoro -> Pirata -> Pirata
saquear formaSaqueo tesoro pirata | formaSaqueo tesoro == True = adquirirTesoro tesoro pirata
 |otherwise = pirata

--saquear anneBonny (saqueoEspecifico "oro") oro
--UnPirata {apodo = "Anne", botin = [UnTesoro {nombre = "oro", valor = 100},UnTesoro {nombre = "doblones", valor = 100},UnTesoro {nombre = "frasco de arena", valor = 1}]}

--saquear davidJones saqueoConCorazon oro
--UnPirata {apodo = "Jones", botin = [UnTesoro {nombre = "cajita musical", valor = 1}]}

--saquear jackSparrow (saqueoComplejo "sombrero") oro
--UnPirata {apodo = "Jack", botin = [UnTesoro {nombre = "brujula que apunta lo que mas deseas", valor = 10000},UnTesoro {nombre = "frasco de arena", valor = 0}]}

----------------------------------------------NAVEGANDO LOS 7 MARES--------------------------------
data Barco = UnBarco {
nombreB :: String,
tripulacion :: [Pirata],
formaSaqueo :: FormaDeSaqueo} deriving (Show)

perlaNegra = UnBarco "perla negra" [jackSparrow, anneBonny] saqueoConCorazon
holandesErrante = UnBarco "holandes errante" [davidJones] saqueoValioso

moneda = UnTesoro "moneda del cofre muerto" 100
espada = UnTesoro "espada de hierro" 50
cuchillo = UnTesoro "cuchillo que le regalo su padre" 5

elizabethSwann = UnPirata "swann" [moneda, espada] --se sube al perla negra
willTurner = UnPirata "will" [cuchillo] --se sube al perla negra pero despues se baja

nuevoPirata :: Pirata -> Barco -> Barco
nuevoPirata pirata barco = barco {tripulacion = pirata:tripulacion barco}

abandonaPirata :: Pirata -> Barco -> Barco
abandonaPirata pirata barco = barco {tripulacion = delete pirata (tripulacion barco)}

--------------------anclar en una isla deshabitada
data Isla = UnaIsla {
nombreI :: String,
elementoTipico :: Tesoro} deriving (Show)

botellaDeRon = UnTesoro "botella de ron" 25

islaTortuga = UnaIsla "Tortuga" frascoDeArena
islaDelRon = UnaIsla "Del Ron" botellaDeRon

atacarIslaDeshabitada :: Isla -> Barco -> Barco
atacarIslaDeshabitada isla barco = barco {tripulacion = map (adquirirTesoro (elementoTipico isla))(tripulacion barco)}

data Ciudad = UnaCiudad {
nombreC :: String,
botinC :: [Tesoro] } deriving (Show)

portRoyal = UnaCiudad "port royal" [brujula, doblones]

atacarCiudad :: Ciudad -> Barco -> Barco
atacarCiudad ciudad barco = barco { tripulacion = zipWith (saquear (formaSaqueo barco)) (botinC ciudad) (tripulacion barco)}

tesorosBarco :: Barco -> [Tesoro]
tesorosBarco barco = concat (map botin (tripulacion barco))

cantPiratas :: Barco -> Int
cantPiratas barco = length(tripulacion barco)

abordarBarco :: Barco -> Barco -> Barco
abordarBarco victima atacante | cantPiratas atacante > cantPiratas victima = atacante {tripulacion = zipWith (saquear (formaSaqueo atacante)) (tesorosBarco victima) (tripulacion atacante)}
 |otherwise = victima

-------------------------------------------------------------------------SEGUNDA PARTE--------------------------------------------------------------------------
