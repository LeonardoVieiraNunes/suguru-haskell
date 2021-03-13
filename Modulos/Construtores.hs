module Modulos.Construtores(Celula, Tabuleiro, setValue, celula, tabuleiro, initTabuleiro, tamanhoTabuleiro, setCands, setCand) where


import Data.Array (Array, array, (//), (!))

type Celula = (Int, Int, [Int])
type Tabuleiro = Array (Int,Int) Celula


tamanhoGrupos :: Int -> Int
tamanhoGrupos 1 = 6
tamanhoGrupos 2 = 6
tamanhoGrupos 3 = 7
tamanhoGrupos 4 = 3
tamanhoGrupos 5 = 6
tamanhoGrupos 6 = 7
tamanhoGrupos 7 = 6
tamanhoGrupos 8 = 5
tamanhoGrupos 9 = 5
tamanhoGrupos 10 = 7
tamanhoGrupos 11 = 7
tamanhoGrupos 12 = 7
tamanhoGrupos 13 = 4
tamanhoGrupos 14 = 6
tamanhoGrupos 15 = 6
tamanhoGrupos 16 = 6
tamanhoGrupos 17 = 6

celula :: (Int, Int) -> Celula
celula (1,1) = (1,2,[])
celula (1,2) = (1,-1,[])
celula (1,3) = (2,1,[])
celula (1,4) = (2,6,[])
celula (1,5) = (2,4,[])
celula (1,6) = (2,3,[])
celula (1,7) = (2,-1,[])
celula (1,8) = (2,2,[])
celula (1,9) = (3,-1,[])
celula (1,10) = (3,3,[])

celula (2,1) = (1,-1,[])
celula (2,2) = (1,5,[])
celula (2,3) = (1,-1,[])
celula (2,4) = (4,-1,[])
celula (2,5) = (4,-1,[])
celula (2,6) = (5,-1,[])
celula (2,7) = (3,1,[])
celula (2,8) = (3,-1,[])
celula (2,9) = (3,6,[])
celula (2,10) = (6,-1,[])

celula (3,1) = (1,6,[])
celula (3,2) = (7,-1,[])
celula (3,3) = (7,6,[])
celula (3,4) = (4,-1,[])
celula (3,5) = (8,-1,[])
celula (3,6) = (5,-1,[])
celula (3,7) = (5,-1,[])
celula (3,8) = (3,5,[])
celula (3,9) = (3,-1,[])
celula (3,10) = (6,4,[])

celula (4,1) = (7,-1,[])
celula (4,2) = (7,1,[])
celula (4,3) = (8,-1,[])
celula (4,4) = (8,-1,[])
celula (4,5) = (8,-1,[])
celula (4,6) = (9,-1,[])
celula (4,7) = (5,-1,[])
celula (4,8) = (5,6,[])
celula (4,9) = (5,-1,[])
celula (4,10) = (6,7,[])

celula (5,1) = (7,3,[])
celula (5,2) = (7,-1,[])
celula (5,3) = (8,2,[])
celula (5,4) = (10,-1,[])
celula (5,5) = (10,-1,[])
celula (5,6) = (9,-1,[])
celula (5,7) = (9,-1,[])
celula (5,8) = (9,-1,[])
celula (5,9) = (9,-1,[])
celula (5,10) = (6,5,[])

celula (6,1) = (11,4,[])
celula (6,2) = (10,-1,[])
celula (6,3) = (10,-1,[])
celula (6,4) = (10,1,[])
celula (6,5) = (10,-1,[])
celula (6,6) = (12,-1,[])
celula (6,7) = (13,2,[])
celula (6,8) = (13,-1,[])
celula (6,9) = (13,4,[])
celula (6,10) = (6,-1,[])

celula (7,1) = (11,-1,[])
celula (7,2) = (10,5,[])
celula (7,3) = (11,-1,[])
celula (7,4) = (11,-1,[])
celula (7,5) = (12,-1,[])
celula (7,6) = (12,7,[])
celula (7,7) = (13,-1,[])
celula (7,8) = (14,-1,[])
celula (7,9) = (6,-1,[])
celula (7,10) = (6,2,[])

celula (8,1) = (11,2,[])
celula (8,2) = (11,1,[])
celula (8,3) = (11,7,[])
celula (8,4) = (12,1,[])
celula (8,5) = (12,3,[])
celula (8,6) = (14,-1,[])
celula (8,7) = (14,2,[])
celula (8,8) = (14,3,[])
celula (8,9) = (15,-1,[])
celula (8,10) = (15,5,[])

celula (9,1) = (16,-1,[])
celula (9,2) = (16,4,[])
celula (9,3) = (16,5,[])
celula (9,4) = (12,-1,[])
celula (9,5) = (12,5,[])
celula (9,6) = (14,-1,[])
celula (9,7) = (14,1,[])
celula (9,8) = (17,-1,[])
celula (9,9) = (15,-1,[])
celula (9,10) = (15,-1,[])

celula (10,1) = (16,1,[])
celula (10,2) = (16,-1,[])
celula (10,3) = (16,3,[])
celula (10,4) = (17,-1,[])
celula (10,5) = (17,-1,[])
celula (10,6) = (17,3,[])
celula (10,7) = (17,-1,[])
celula (10,8) = (17,2,[])
celula (10,9) = (15,1,[])
celula (10,10) = (15,2,[])

-- celula de controle
celula (-1,-1) = (-1,-1,[-1])

initTabuleiro :: Tabuleiro
initTabuleiro = tabuleiro (array ((1,1), (10,10)) [((x,y),celula(x,y)) | x<-[1..10], y<-[1..10]])

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
