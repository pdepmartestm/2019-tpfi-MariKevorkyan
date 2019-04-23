type Nombre = [Char]
type Valor = Integer

type Tesoro = (Nombre, Valor)
type Botin = [Tesoro]
type Pirata = (Nombre, Botin)

------------------cant tesoros pirata---------------------
cantTesorosPirata :: Pirata -> Int
cantTesorosPirata (_, botin) = length botin

------------------pirata afortunado------------------
valorTesoro :: Tesoro->Valor
valorTesoro (_,valor) = valor

listaValorBotin :: Botin -> [Valor]
listaValorBotin botin = map valorTesoro botin

valorTotalBotin :: Botin -> Valor
valorTotalBotin botin = sum (listaValorBotin botin)

pirataAfortunado :: Pirata -> Bool
pirataAfortunado (_, botin) = valorTotalBotin botin > 10000

----------------piratas con mismo tesoro pero valor diferente-------------
mismoTesoroDistinoValor :: Tesoro->Tesoro->Bool
mismoTesoroDistinoValor (nombre1,valor1) (nombre2,valor2) = (nombre1==nombre2) && (valor1/=valor2)

mismoTesoro1Pirata2 :: Tesoro->Pirata->Bool
mismoTesoro1Pirata2 tesoro1 (_,botin) = any (mismoTesoroDistinoValor tesoro1) botin

---------------------valor del tesoro más valioso de un pirata------------------
valoresPirata :: Pirata -> Valor
valoresPirata (nombre,botin) = maximum (listaValorBotin botin)

-------------------------pirata luego de adquirir un nuevo tesoro---------------------
nuevoTesoro :: Pirata-> Tesoro->Pirata
nuevoTesoro (nombre, botin) tesoro = (nombre, botin ++ [tesoro])

------------------pirata luego de perder todos los tesoros valiosos---------------
noEsTesoroValioso :: Tesoro -> Bool
noEsTesoroValioso (nombre, valor) = valor <= 100

perderValiosos :: Pirata->Pirata
perderValiosos (nombre, botin) = (nombre, filter noEsTesoroValioso botin)

-----------pirata luego de perder todos los tesoros con un nombre dado-------------
diferenteNombre :: Tesoro->Nombre->Bool
diferenteNombre (nombre, valor) unNombre = nombre /= unNombre

--perderTesoroNombre :: Pirata->Nombre->Pirata ------------!!!!!!!!!!!!!!!!!!!!!!!!
--perderTesoroNombre (nombre, botin) nombree = (nombre, filter (diferenteNombre nombre, nombree) botin )

------------------------EJEMPLOS----------------------------
ejemplo1 = ("Jack Sparrow", [("Brujula", 10000), ("frasco de arena", 0)])
ejemplo2= ("David Jones", [("cajita musical", 1)])
ejemplo3 = ("Anne Bony", [("Doblones", 100), ("frasco de arena", 1)])

-----------------TEMPORADA DE SAQUEOS--------------
type Saqueo = (Tesoro->Bool)

saqueoValioso :: Tesoro->Bool
saqueoValioso tesoro = not(noEsTesoroValioso tesoro)

saqueoObjetoEspecifico :: Tesoro->Nombre->Bool
saqueoObjetoEspecifico (nombre,valor) objeto = nombre == objeto

saqueoConCorazon :: Tesoro->Bool
saqueoConCorazon tesoro = False

saqueoComplejo :: Tesoro->Nombre->Bool
saqueoComplejo tesoro objeto = saqueoValioso tesoro || saqueoObjetoEspecifico tesoro objeto

saquear :: Saqueo->Pirata->Tesoro->Pirata
saquear formaDeSaqueo (nombre,botin) tesoro = (nombre, botin ++ (filter formaDeSaqueo [tesoro]))

--------------------NAVEGANDO LOS SIETE MARES--------------------
type Tripulacion = [Pirata]
type Barco = (Nombre,Tripulacion, Saqueo)
type ElementoTipico = Tesoro
type Isla = (Nombre, ElementoTipico)
type Ciudad = (Nombre, Botin)

nuevoPirata :: Barco->Pirata->Barco
nuevoPirata  (nombre, tripulacion, saqueo) pirata = (nombre, tripulacion ++ [pirata], saqueo)

abandonaPirata :: Barco->Pirata->Barco
abandonaPirata (nombre, tripulacion, saqueo) pirata = (nombre, filter (/=pirata) tripulacion, saqueo)

--ejemplo4 = ("Perla Negra", [ejemplo1, ejemplo3], _)
--ejemplo5 = ("Holandes Errante", [ejemplo2], _)
--ejemplo6 = ("Elizabeth Swann", [("moneda del cofre muerto", 100), ("espada de hierro", 50)]) --se sube al ejemplo4
--ejemplo7 = ("Will Turner", ([("cuchillo que le regalo el padre", 5)]) --se sube al ejemplo4 pero luego abandona

anclarEnIslaDeshabitada :: Barco -> Isla -> Barco
anclarEnIslaDeshabitada (nombre, tripulacion, saqueo) (nombreIsla, elementotipico) = (nombre, map (saquear saqueo elementotipico) tripulacion, saqueo)

atacarUnaCiudad :: Ciudad->Barco->Barco
atacarUnaCiudad (nombre, botin) (nombreB, tripulacion, saqueo) = (nombreB, zipWith (saquear saqueo) botin tripulacion , saqueo)

--abordarOtroBarcoEnAltamar ::
--abordarOtroBarcoEnAltamar


