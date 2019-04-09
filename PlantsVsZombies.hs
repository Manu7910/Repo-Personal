data Planta = UnaPlanta {
 especie :: String,
 ptosDevida :: Int,
 solesProducidos :: Int, 
 poderDeAtaque :: Int
}

peaShooter = UnaPlanta {
 especie = especialidad peaShooter,
 ptosDevida = 5,
 solesProducidos = 0,
 poderDeAtaque = 2
}

repeater = UnaPlanta {
 especie = especialidad repeater,
 ptosDevida = 5,
 solesProducidos = 0,
 poderDeAtaque = (*2).poderDeAtaque $ peaShooter
}

sunflower = UnaPlanta {
 especie = especialidad sunflower,
 ptosDevida = 7,
 solesProducidos = 1,
 poderDeAtaque = 0
}

especialidad :: Planta -> String
especialidad planta
 | (solesProducidos planta) > 0 = "Proveedora"
 | ((*2).poderDeAtaque $ planta) > (ptosDevida planta) = "Atacante" 
 | otherwise = "Defensiva" 
