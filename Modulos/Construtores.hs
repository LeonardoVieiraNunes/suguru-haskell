module Modulos.Construtores(Celula, Valor, Tabuleiro, Candidatos,setCand,celula, tabuleiro, setValue,initTabuleiro,tamanhoTabuleiro, setCands) where

import Data.Array (Array, array, (//), (!))

type IdGrupo = Int
type Valor = Int
type Candidatos = [Int]
type Celula = (Int, Int, [Int])
type Tabuleiro = Array (Int,Int) Celula


tamanhoGrupos :: Int -> Int
tamanhoGrupos 1 = 6
tamanhoGrupos 2 = 6
tamanhoGrupos 3 = 1
tamanhoGrupos 4 = 5
tamanhoGrupos 5 = 6
tamanhoGrupos 6 = 6
tamanhoGrupos 7 = 6
tamanhoGrupos 8 = 5
tamanhoGrupos 9 = 3
tamanhoGrupos 10 = 5

celula :: (Int, Int) -> Celula
celula (1,1) = (1,5,[])
celula (1,2) = (1,-1,[])
celula (1,3) = (2,5,[])
celula (1,4) = (2,-1,[])
celula (1,5) = (2,1,[])
celula (1,6) = (2,-1,[])
celula (1,7) = (3,-1,[])

celula (2,1) = (1,-1,[])
celula (2,2) = (1,3,[])
celula (2,3) = (4,-1,[])
celula (2,4) = (4,-1,[])
celula (2,5) = (4,-1,[])
celula (2,6) = (2,4,[])
celula (2,7) = (2,-1,[])

celula (3,1) = (1,6,[])
celula (3,2) = (1,2,[])
celula (3,3) = (5,-1,[])
celula (3,4) = (5,-1,[])
celula (3,5) = (4,5,[])
celula (3,6) = (6,6,[])
celula (3,7) = (6,2,[])

celula (4,1) = (7,5,[])
celula (4,2) = (7,-1,[])
celula (4,3) = (5,3,[])
celula (4,4) = (5,2,[])
celula (4,5) = (4,-1,[])
celula (4,6) = (6,-1,[])
celula (4,7) = (6,-1,[])

celula (5,1) = (7,3,[])
celula (5,2) = (7,-1,[])
celula (5,3) = (5,4,[])
celula (5,4) = (5,-1,[])
celula (5,5) = (6,-1,[])
celula (5,6) = (6,-1,[])
celula (5,7) = (8,2,[])

celula (6,1) = (7,-1,[])
celula (6,2) = (9,1,[])
celula (6,3) = (9,-1,[])
celula (6,4) = (9,-1,[])
celula (6,5) = (8,-1,[])
celula (6,6) = (8,-1,[])
celula (6,7) = (8,-1,[])

celula (7,1) = (7,6,[])
celula (7,2) = (10,-1,[])
celula (7,3) = (10,4,[])
celula (7,4) = (10,-1,[])
celula (7,5) = (10,3,[])
celula (7,6) = (10,-1,[])
celula (7,7) = (8,5,[])
<<<<<<< HEAD

=======
>>>>>>> 4079c697458c531ce50986b81d65453ccf7c5f27
-- celula de controle
celula (-1,-1) = (-1,-1,[-1])

initTabuleiro :: Tabuleiro
initTabuleiro = tabuleiro (array ((1,1), (7,7)) [
    ((1,1), celula(1,1)), ((1,2), celula(1,2)),((1,3), celula(1,3)),((1,4), celula(1,4)),((1,5), celula(1,5)),((1,6), celula(1,6)), ((1,7), celula(1,7)),
    ((2,1), celula(2,1)), ((2,2), celula(2,2)),((2,3), celula(2,3)),((2,4), celula(2,4)),((2,5), celula(2,5)),((2,6), celula(2,6)), ((2,7), celula(2,7)),
    ((3,1), celula(3,1)), ((3,2), celula(3,2)),((3,3), celula(3,3)),((3,4), celula(3,4)),((3,5), celula(3,5)),((3,6), celula(3,6)), ((3,7), celula(3,7)),
    ((4,1), celula(4,1)), ((4,2), celula(4,2)),((4,3), celula(4,3)),((4,4), celula(4,4)),((4,5), celula(4,5)),((4,6), celula(4,6)), ((4,7), celula(4,7)),
    ((5,1), celula(5,1)), ((5,2), celula(5,2)),((5,3), celula(5,3)),((5,4), celula(5,4)),((5,5), celula(5,5)),((5,6), celula(5,6)), ((5,7), celula(5,7)),
    ((6,1), celula(6,1)), ((6,2), celula(6,2)),((6,3), celula(6,3)),((6,4), celula(6,4)),((6,5), celula(6,5)),((6,6), celula(6,6)), ((6,7), celula(6,7)),
    ((7,1), celula(7,1)), ((7,2), celula(7,2)),((7,3), celula(7,3)),((7,4), celula(7,4)),((7,5), celula(7,5)),((7,6), celula(7,6)), ((7,7), celula(7,7))
    ])

tabuleiro :: Tabuleiro -> Tabuleiro
tabuleiro tb = tb

tamanhoTabuleiro :: Tabuleiro -> Int 
tamanhoTabuleiro tb = round (sqrt (fromIntegral (length (tabuleiro tb))))

changeValorCelula :: Int -> Int -> Int -> Celula
changeValorCelula i j nVal =
    let (id,val,cand) = celula (i,j)
    in (id,nVal,cand)

setValue :: (Int, Int) -> Int -> Tabuleiro -> Tabuleiro
setValue (x,y) a tb = tb // [((x,y), changeValorCelula x y a)]

changeCandsCelula :: Int -> Int -> [Int] -> Celula
changeCandsCelula i j nCand =
    let (id,val,cand) = celula (i,j)
    in (id,val,nCand)

changeCandCelula :: Int -> Int -> Int -> Celula
changeCandCelula i j nCand =
    let (id,val,cand) = celula (i,j)
    in (id,nCand,cand)

setCands :: (Int, Int) -> [Int] -> Tabuleiro -> Tabuleiro
setCands (x,y) a tb = tb // [((x,y), changeCandsCelula x y a)]

setCand :: (Int,Int) -> Int -> Tabuleiro -> Tabuleiro
setCand (x,y) cand tb = tb // [((x,y), changeCandCelula x y cand)]
