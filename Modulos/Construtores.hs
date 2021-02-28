module Modulos.Construtores(Celula, Tabuleiro, Candidatos, celula, tabuleiro, setValue,initTabuleiro,tamanhoTabuleiro) where

import Data.Array (Array, array, (//))

type IdGrupo = Int
type Valor = Int
type Candidatos = [Int]
type Celula = (Int, Int, [Int])
type Tabuleiro = Array (Int,Int) Celula

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

initTabuleiro :: Tabuleiro
initTabuleiro = tabuleiro (array ((1,1), (4,4)) [
    ((1,1), celula(1,1)), ((1,2), celula(1,2)),((1,3), celula(1,3)),((1,4), celula(1,4)),
    ((2,1), celula(2,1)), ((2,2), celula(2,2)),((2,3), celula(2,3)),((2,4), celula(2,4)),
    ((3,1), celula(3,1)), ((3,2), celula(3,2)),((3,3), celula(3,3)),((3,4), celula(3,4)),
    ((4,1), celula(4,1)), ((4,2), celula(4,2)),((4,3), celula(4,3)),((4,4), celula(4,4))
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