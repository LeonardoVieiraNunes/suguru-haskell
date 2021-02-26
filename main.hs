type IdGrupo = Int
type TamanhoGrupo = Int
type ValorCelula = Int

type Celula = (IdGrupo, TamanhoGrupo, ValorCelula)
type Tabuleiro = [[Celula]]

celula :: Int -> Int -> Celula
celula 0 1 = (1, 1, 1)
celula 0 2 = (2, 5, 5)
celula 0 3 = (3, 4, 2)
celula 0 4 = (3, 4, 1)

celula 1 1 = (2, 5, 2)
celula 1 2 = (2, 5, 3)
celula 1 3 = (2, 5, 4)
celula 1 4 = (3, 4, 3)

celula 2 1 = (4, 2, 1)
celula 2 2 = (2, 5, 5)
celula 2 3 = (5, 4, 1)
celula 2 4 = (3, 4, 2)

celula 3 1 = (4, 2, 2)
celula 3 2 = (5, 4, 4)
celula 3 3 = (5, 4, 3)
celula 3 4 = (5, 4, 4)


tabuleiro :: Tabuleiro
tabuleiro = [[celula 0 1, celula 0 2, celula 0 3, celula 0 4], [celula 1 1, celula 1 2, celula 1 3, celula 1 4], [celula 2 1, celula 2 2, celula 2 3, celula 2 4], [celula 3 1, celula 3 2, celula 3 3, celula 3 4]]

sizeTabuleiro :: Int
sizeTabuleiro = length tabuleiro

findByIndex :: Int -> Int -> Celula
findByIndex i j = (tabuleiro!!i)!!j

isGruposLinhaInvalida :: [Celula] -> Bool
isGruposLinhaInvalida linha = False `elem` map (\(_,tam,val) -> val <= tam) linha

isGruposTabuleiroInvalido :: Bool
isGruposTabuleiroInvalido = any isGruposLinhaInvalida tabuleiro

-- adjacentes :: Celula -> [ValorCelula]
-- adjacentes cel = 

main = do
    print tabuleiro
    print (findByIndex 3 3)
    print sizeTabuleiro
    print isGruposTabuleiroInvalido
    -- print (isGruposLinhaInvalida (tabuleiro!!0))
    -- print (percorre 0 0)