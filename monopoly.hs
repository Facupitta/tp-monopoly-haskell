data Jugador = UnJugador {nombre :: Nombre, dinero :: Dinero, tactica :: Tactica, propiedades :: [Propiedad], acciones :: [Accion]}

type Nombre = String

type Dinero = Int

type Precio = Int

type Tactica = String

compradorCompulsivo :: Tactica
compradorCompulsivo = "Comprador compulsivo"

oferenteSingular :: Tactica
oferenteSingular = "Oferente Singular"

accionista :: Tactica
accionista = "Accionista"

type Propiedad = (Nombre, Precio)

precioDePropiedad :: Propiedad -> Precio
precioDePropiedad propiedad = snd propiedad

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata unaPropiedad = precioDePropiedad unaPropiedad < 150

--Estas casas las hice para probar
casaNormal :: Propiedad
casaNormal = ("Casa Normal", 250)

casaBarata :: Propiedad
casaBarata = ("Casa Barata", 100)

casaCara :: Propiedad
casaCara = ("Casa Cara", 500)

edificio :: Propiedad
edificio = ("Edificio", 1000)

mansion :: Propiedad
mansion = ("Mansion", 2500)


type Accion = (Jugador -> Jugador)

cambiarDinero :: (Dinero -> Dinero -> Dinero) -> Dinero -> Accion
cambiarDinero operacion nuevoDinero unJugador = unJugador {dinero = operacion (dinero unJugador) nuevoDinero}

cambiarTactica :: Tactica -> Accion
cambiarTactica nuevaTactica unJugador = unJugador {tactica = nuevaTactica}

concatenarAlNombre :: Nombre -> Accion
concatenarAlNombre palabraAgregada unJugador = unJugador {nombre = palabraAgregada ++ (nombre unJugador)}

agregarAccion :: Accion -> Accion
agregarAccion nuevaAccion unJugador = unJugador {acciones = nuevaAccion : (acciones unJugador)}

agregarPropiedad :: Propiedad -> Accion
agregarPropiedad nuevaPropiedad unJugador = unJugador {propiedades = nuevaPropiedad : (propiedades unJugador)}

comprarPropiedad :: Propiedad -> Accion
comprarPropiedad nuevaPropiedad unJugador = (agregarPropiedad nuevaPropiedad.cambiarDinero (-) (precioDePropiedad nuevaPropiedad)) unJugador

esSuTactica :: Tactica -> Jugador -> Bool
esSuTactica tacticaAProbar unJugador = tactica unJugador == tacticaAProbar

pasarPorElBanco :: Accion
pasarPorElBanco unJugador = (cambiarTactica compradorCompulsivo.cambiarDinero (+) 40) unJugador

gritar :: Accion
gritar unJugador = concatenarAlNombre "AHHHH" unJugador

enojarse :: Accion
enojarse unJugador = (agregarAccion gritar.cambiarDinero (+) 50) unJugador

llegaAComprar :: Propiedad -> Jugador -> Bool
llegaAComprar unaPropiedad unJugador = (dinero unJugador >= precioDePropiedad unaPropiedad)

puedeSubastar :: Propiedad -> Jugador -> Bool
puedeSubastar unaPropiedad unJugador = (tactica unJugador == oferenteSingular || tactica unJugador == accionista) && llegaAComprar unaPropiedad unJugador

subastar :: Propiedad -> Accion
subastar unaPropiedad unJugador | puedeSubastar unaPropiedad unJugador = comprarPropiedad unaPropiedad unJugador
                                | otherwise = unJugador

cantidadDePropiedadesSegun :: (Propiedad -> Bool) -> Jugador -> Int
cantidadDePropiedadesSegun condicion unJugador = (length.filter condicion.propiedades) unJugador

totalDeAlquileres :: Jugador -> Dinero
totalDeAlquileres unJugador = ((*10).cantidadDePropiedadesSegun esPropiedadBarata) unJugador + ((*20).cantidadDePropiedadesSegun (not.esPropiedadBarata)) unJugador

cobrarAlquileres :: Accion
cobrarAlquileres unJugador = cambiarDinero (+) (totalDeAlquileres unJugador) unJugador

pagarAAccionistas :: Accion
pagarAAccionistas unJugador | esSuTactica accionista unJugador = cambiarDinero (+) 200 unJugador
                            | otherwise = cambiarDinero (-) 100 unJugador

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor unaPropiedad unJugador    | llegaAComprar unaPropiedad unJugador = comprarPropiedad unaPropiedad unJugador
                                            | otherwise = hacerBerrinchePor unaPropiedad ((gritar.cambiarDinero (+) 10) unJugador)

ultimaRonda :: Jugador -> Accion
ultimaRonda unJugador = foldl1 (.) (acciones unJugador)

juegoFinal :: Jugador -> Jugador -> Jugador
juegoFinal primerJugador segundoJugador | (dinero.ultimaRonda primerJugador) primerJugador > (dinero.ultimaRonda segundoJugador) segundoJugador = primerJugador
                                        | otherwise = segundoJugador

carolina :: Jugador
carolina = UnJugador "Carolina" 500 accionista [] [pasarPorElBanco, pagarAAccionistas]

manuel :: Jugador
manuel = UnJugador "Manuel" 500 oferenteSingular [] [pasarPorElBanco, enojarse]