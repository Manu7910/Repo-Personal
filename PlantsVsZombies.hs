--1a)
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
--1b) 
data Zombie = UnZombie {
 nombre :: String,
 accesorios :: [String],
 danioPorMordida :: Int,
 nivelDeMuerte :: Int
}

zombieBase = UnZombie {
 nombre = "Zombie",
 accesorios = [],
 danioPorMordida = 1,
 nivelDeMuerte = saberNivelDeMuerte zombieBase 
}

balloonZombie = UnZombie {
 nombre = "Pepe Colgado",
 accesorios = ["Globo"],
 danioPorMordida = 1,
 nivelDeMuerte = saberNivelDeMuerte balloonZombie
}

newspaperZombie = UnZombie {
 nombre = "Beto el chismoso",
 accesorios = ["Diario"],
 danioPorMordida = 2,
 nivelDeMuerte = saberNivelDeMuerte newspaperZombie
}

gargantuar = UnZombie {
 nombre = "Gargantuar Hulk Smash Puny God",
 accesorios = ["Poste","Enano"],
 danioPorMordida = 30,
 nivelDeMuerte = saberNivelDeMuerte gargantuar
}

saberNivelDeMuerte :: Zombie -> Int
saberNivelDeMuerte = length.nombre

--2a)
especialidad :: Planta -> String
especialidad planta
 | (solesProducidos planta) > 0 = "Proveedora"
 | ((*2).poderDeAtaque $ planta) > (ptosDevida planta) = "Atacante" 
 | otherwise = "Defensiva"
--2b)
esPeligroso :: Zombie -> Bool
esPeligroso zombie = (length.accesorios $ zombie) > 1 || (nivelDeMuerte zombie) > 10
--3a)
data LineaDeDefensa = UnaLineaDeDefensa {
 plantas :: [Planta],
 zombies :: [Zombie]
}

linea1 = LineaDeDefensa {
 plantas = [sunflower, sunflower, sunflower],
 zombies = []
}
linea2 = LineaDeDefensa {
 plantas = [peaShooter, peaShooter, sunflower, nut],
 zombies = [zombieBase, newspaperZombie]
}
linea3 = LineaDeDefensa {
 plantas = [sunflower, peaShooter],
 zombies = [gargantuar, zombieBase, zombieBase]
}
linea4 = LineaDeDefensa {
 plantas = [peaShooter],
 zombies = [zombieBase]
}

agregarPlanta :: LineaDeDefensa -> Planta -> LineaDeDefensa
agregarPlanta linea planta = 
