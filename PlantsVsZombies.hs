data Planta = UnaPlanta {
 ptosDevida :: Int,
 solesProducidos :: Int, 
 poderDeAtaque :: Int
}

peaShooter = UnaPlanta {
 ptosDevida = 5,
 solesProducidos = 0,
 poderDeAtaque = 2
}

repeater = UnaPlanta {
 ptosDevida = 5,
 solesProducidos = 0,
 poderDeAtaque = (*2).poderDeAtaque $ peaShooter
}

sunflower = UnaPlanta {
 ptosDevida = 7,
 solesProducidos = 1,
 poderDeAtaque = 0
}

especialidad :: Planta -> String
especialidad planta
 | (solesProducidos planta) > 0 = "Proveedora"
 | ((*2).poderDeAtaque $ planta) > (ptosDevida planta) = "Atacante" 
 | otherwise = "Defensiva" 
