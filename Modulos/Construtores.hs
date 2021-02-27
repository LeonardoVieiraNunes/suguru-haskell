module Modulos.Construtores(Celula, Tabuleiro, Candidatos, celula, tabuleiro) where

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