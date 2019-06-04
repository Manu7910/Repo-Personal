--1)

data Reserva = UnaReserva {
	pasajeros :: [String],
	tramos :: [(String, String, Float)],
	agregados :: [Reserva -> Float],
	costoBase :: Float
}

--2)

reservaLarga = UnaReserva {
	pasajeros = ["Tom Borenstyn", "Frank Gorek"],
	tramos = [("Bs As", "San Pablo", 6), ("San Pablo", "Londres" , 10)],
	agregados = [(otrosUtensilios 3)],
	costoBase = 45000
}

reservaCorta = UnaReserva {
	pasajeros = ["Cesar Frere"],
	tramos = [("Bs As", "Chascomus", 0.25)], -- 15 min es un cuarto de hora -> 0.25
	agregados = [(otrosUtensilios 6), lunchCompleto, (equipajeExtra 3)],
	costoBase = 50000
}

--3)

lunchCompleto = (*0.15).costoBase

menuEspecial descripcion _ = (*10).(length descripcion)

equipajeExtra valijas _ = (*200).valijas

otrosUtensilios cantidad  = (/100).(*cantidad).costoBase

polizon _ _ = 0

--4)

sumarAgregado agregado reserva = reserva {agregados = (agregados reserva) : agregado}

sumarMonto monto reserva = reserva {costoBase = (costoBase reserva) + monto}

sumarTramo tramo reserva = reserva {tramos = (tramos reserva) : tramo}

--5)

precioTotal reserva = (+(costoBase reserva)).sum.(map ($ reserva)).agregados $ reserva  

--6)

duracion (_,_, d) = d

esLarga reserva = (sum.(map (duracion)).tramos reserva) > 15

--7)

nuevaEscala tramo costo  = (sumarMonto monto).(sumarTramo tramo)

--8)

origen (o, _, _) = o

destino (_, d, _) = d


bienConstruida  = coinciden.tramos

coinciden [] = []
coinciden (x:xs) = (destino x) == origen (coinciden xs)  

--9)

precioTotal.(sumarAgregado (otrosUtensilios 2)) $ reservaLarga

bienConstruida.(nuevaEscala ("Barcelona", "Roma", 2) 4000).(nuevaEscala ("Miami", "Barcelona", 11) 15000) $ reservaLarga
