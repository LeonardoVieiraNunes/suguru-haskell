type IdGrupo = Int
type Valor = Int
type Candidatos = [Int]
type Celula = (IdGrupo, Valor, Candidatos)
type Tabuleiro = [[Celula]]

celula :: (Int, Int) -> Celula
celula (1,1) = (1,1,[])
celula (1,2) = (2,-1,[])
celula (1,3) = (3,-1,[])
celula (1,4) = (3,-1,[])

celula (2,1) = (2,-1,[])
celula (2,2) = (2,-1,[])
celula (2,3) = (2,4,[])
celula (2,4) = (3,-1,[])

celula (3,1) = (4,-1,[])
celula (3,2) = (2,-1,[])
celula (3,3) = (5,-1,[])
celula (3,4) = (3,-1,[])

celula (4,1) = (4,2,[])
celula (4,2) = (5,-1,[])
celula (4,3) = (5,3,[])
celula (4,4) = (5,-1,[])
-- celula de controle
celula (-1,-1) = (-1,-1,[-1])


tabuleiro :: Tabuleiro
tabuleiro = [
    [celula (1,1), celula (1,2), celula (1,3), celula (1,4)],
    [celula (2,1), celula (2,2), celula (2,3), celula (2,4)],
    [celula (3,1), celula (3,2), celula (3,3), celula (3,4)],
    [celula (4,1), celula (4,2), celula (4,3), celula (4,4)]
    ]

getCand :: Celula -> Candidatos
getCand (id, val, cand) = cand

getCelulaEsq :: Int -> Int -> (Int, Int)
getCelulaEsq i j | i-1 > 0 = (i-1, j)
                 | otherwise = (-1,-1)

getCelulaDir :: Int -> Int -> (Int, Int)
getCelulaDir i j | i < length tabuleiro - 1 = (i+1, j)
                 | otherwise = (-1, -1)

getCelulaCima :: Int -> Int -> (Int, Int)
getCelulaCima i j | j-1 > 0 = (i, j-1)
                  | otherwise = (-1,-1)

getCelulaBaixo :: Int -> Int -> (Int, Int)
getCelulaBaixo i j | j < length tabuleiro - 1 = (i, j+1)
                   | otherwise = (-1,-1)

getCelulaDiagEsqCima :: Int -> Int -> (Int, Int)
getCelulaDiagEsqCima i j | i-1 > 0 && j-1 > 0 = (i-1, j-1)
                         | otherwise = (-1,-1)

getCelulaDiagDirCima :: Int -> Int -> (Int, Int)
getCelulaDiagDirCima i j | i < length tabuleiro - 1 && j-1 > 0 = (i+1, j+1)
                         | otherwise = (-1,-1)

getCelulaDiagEsqBaixo :: Int -> Int -> (Int, Int)
getCelulaDiagEsqBaixo i j | i-1 > 0 && j < length tabuleiro - 1 = (i-1, j+1)
                          | otherwise = (-1,-1)

getCelulaDiagDirBaixo :: Int -> Int -> (Int, Int)
getCelulaDiagDirBaixo i j | i < length tabuleiro - 1 && j < length tabuleiro - 1 = (i+1, j+1)
                          | otherwise = (-1,-1)

getCandAdjacentes :: Int -> Int -> [(Int, Int)]
getCandAdjacentes i j = [getCelulaDiagEsqCima i j, getCelulaCima i j, getCelulaDiagDirCima i j, getCelulaEsq i j, getCelulaDir i j, getCelulaDiagEsqBaixo i j, getCelulaBaixo i j, getCelulaDiagDirBaixo i j]


main = do
    print (getCandAdjacentes 1 1)