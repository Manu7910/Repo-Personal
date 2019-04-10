--1a)
data Planta = UnaPlanta {
 ptosDevida :: Int,
 solesProducidos :: Int, 
 poderDeAtaque :: Int
} deriving (Show)

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

nut = UnaPlanta {
 ptosDevida = 100,
 solesProducidos = 0,
 poderDeAtaque = 0
}

--1b) 
data Zombie = UnZombie {
 nombre :: String,
 accesorios :: [String],
 danioPorMordida :: Int,
 nivelDeMuerte :: Int
} deriving (Show)

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
} deriving (Show)

linea1 = UnaLineaDeDefensa {
 plantas = [sunflower, sunflower, sunflower],
 zombies = []
}

linea2 = UnaLineaDeDefensa {
 plantas = [peaShooter, peaShooter, sunflower, nut],
 zombies = [zombieBase, newspaperZombie]
}

linea3 = UnaLineaDeDefensa {
 plantas = [sunflower, peaShooter],
 zombies = [gargantuar, zombieBase, zombieBase]
}

linea4 = UnaLineaDeDefensa {
 plantas = [peaShooter],
 zombies = [zombieBase]
}

agregarPlanta :: LineaDeDefensa -> Planta -> LineaDeDefensa
agregarPlanta linea planta = linea{plantas = agregarAfila (plantas linea) planta}

agregarZombie :: LineaDeDefensa -> Zombie -> LineaDeDefensa
agregarZombie linea zombie = linea{zombies = agregarAfila (zombies linea) zombie}

agregarAfila :: [a] -> a -> [a]
agregarAfila linea plantaOzombie = linea++[plantaOzombie]

--3b)
estaEnPeligro :: LineaDeDefensa -> Bool
estaEnPeligro linea = (poderDeAtaqueTotal linea) < (danioPorMordidaTotal linea) || sonTodosPeligrosos linea

poderDeAtaqueTotal :: LineaDeDefensa -> Int
poderDeAtaqueTotal linea = sum.(map poderDeAtaque) $ (plantas linea)

danioPorMordidaTotal :: LineaDeDefensa -> Int
danioPorMordidaTotal linea = sum.(map danioPorMordida) $ (zombies linea)

sonTodosPeligrosos :: LineaDeDefensa -> Bool
sonTodosPeligrosos linea = all esPeligroso (zombies linea) 

--3c)
necesitaSerDefendida :: LineaDeDefensa -> Bool
necesitaSerDefendida linea = all ((== "Proveedora").especialidad) (plantas linea)

--3d)

--4)
distintaEspecialidad :: Planta -> Planta -> Bool
distintaEspecialidad planta1 planta2 = (especialidad planta1) /= (especialidad planta2)

compararEspecialidad :: [Planta] -> Bool
compararEspecialidad [] = True
compararEspecialidad [x] = False
compararEspecialidad (x:y:ys) = (distintaEspecialidad x y) && (compararEspecialidad ys)

esMixta :: LineaDeDefensa -> Bool
esMixta linea = compararEspecialidad (plantas linea)

--5a)
ataqueDePlanta :: Planta -> Zombie -> Zombie
ataqueDePlanta planta zombie = zombie{nombre = (drop (poderDeAtaque planta)).nombre $ zombie } 

--5b)
ataqueDeZombie :: Zombie -> Planta -> Planta
ataqueDeZombie zombie planta = planta{ptosDevida = (ptosDevida planta) - (danioPorMordida zombie) }