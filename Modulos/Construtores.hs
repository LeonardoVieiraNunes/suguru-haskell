module Modulos.Construtores(Celula, Tabuleiro, setValue, celula, tabuleiro, initTabuleiro, tamanhoTabuleiro, setCands, setCand) where


import Data.Array (Array, array, (//), (!))

type Celula = (Int, Int, [Int])
type Tabuleiro = Array (Int,Int) Celula


tamanhoGrupos :: Int -> Int
tamanhoGrupos 1 = 6
tamanhoGrupos 2 = 4
tamanhoGrupos 3 = 5
tamanhoGrupos 4 = 6
tamanhoGrupos 5 = 5
tamanhoGrupos 6 = 5
tamanhoGrupos 7 = 3
tamanhoGrupos 8 = 5
tamanhoGrupos 9 = 5
tamanhoGrupos 10 = 4
tamanhoGrupos 11 = 1

celula :: (Int, Int) -> Celula
celula (1,1) = (1,3,[])
celula (1,2) = (1,4,[])
celula (1,3) = (1,-1,[])
celula (1,4) = (1,6,[])
celula (1,5) = (1,-1,[])
celula (1,6) = (2,3,[])
celula (1,7) = (2,1,[])

celula (2,1) = (3,2,[])
celula (2,2) = (3,-1,[])
celula (2,3) = (4,-1,[])
celula (2,4) = (4,-1,[])
celula (2,5) = (1,-1,[])
celula (2,6) = (2,-1,[])
celula (2,7) = (2,2,[])

celula (3,1) = (3,-1,[])
celula (3,2) = (3,3,[])
celula (3,3) = (4,-1,[])
celula (3,4) = (4,6,[])
celula (3,5) = (4,-1,[])
celula (3,6) = (4,-1,[])
celula (3,7) = (5,-1,[])

celula (4,1) = (3,4,[])
celula (4,2) = (6,-1,[])
celula (4,3) = (6,5,[])
celula (4,4) = (5,-1,[])
celula (4,5) = (5,5,[])
celula (4,6) = (5,-1,[])
celula (4,7) = (5,-1,[])

celula (5,1) = (6,-1,[])
celula (5,2) = (6,-1,[])
celula (5,3) = (6,-1,[])
celula (5,4) = (7,-1,[])
celula (5,5) = (7,-1,[])
celula (5,6) = (7,-1,[])
celula (5,7) = (8,-1,[])

celula (6,1) = (9,5,[])
celula (6,2) = (9,-1,[])
celula (6,3) = (9,-1,[])
celula (6,4) = (9,3,[])
celula (6,5) = (8,4,[])
celula (6,6) = (8,-1,[])
celula (6,7) = (8,-1,[])

celula (7,1) = (9,4,[])
celula (7,2) = (10,3,[])
celula (7,3) = (10,4,[])
celula (7,4) = (10,-1,[])
celula (7,5) = (10,-1,[])
celula (7,6) = (11,1,[])
celula (7,7) = (8,3,[])
-- celula de controle
celula (-1,-1) = (-1,-1,[-1])

initTabuleiro :: Tabuleiro
initTabuleiro = tabuleiro (array ((1,1), (7,7)) [((x,y),celula(x,y)) | x<-[1..7], y<-[1..7]])

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
