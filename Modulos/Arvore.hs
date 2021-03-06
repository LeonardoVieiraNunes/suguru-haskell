module Modulos.Arvore (Arvore) where
import Data.Array (Array, (!))
import Modulos.OperacoesMatriz ( getPosAdjacentes, preencherCandidatos, isCandidato, getCand, getCelulaPos )
import Modulos.Construtores(Tabuleiro, Celula, tabuleiro, celula, setValue, initTabuleiro, tamanhoTabuleiro, setCands)

data Arvore a = Null | No Tabuleiro (Arvore a) (Arvore a) (Arvore a)
    deriving (Show,Eq)

minhaArvore :: Arvore a
minhaArvore = No initTabuleiro Null Null Null

tabArvore ::  Arvore a -> Tabuleiro
tabArvore (No tb esq dir cima) = tb
--arvore onde sera inserido(nulo caso seja folha) -> tabuleiro que sera inserido -> retorna arvore inicial
inserirArvore :: (Ord a) => Arvore a -> Bool -> Tabuleiro -> Arvore a -> Arvore a
inserirArvore Null _ tb = No tb Null Null
inserirArvore (No tab esq dir cima) menor tb
            |tab == tb = No tab esq dir
            |not menor = No tab esq (inserirArvore dir False tb (No tab esq dir cima ))
            |otherwise = No tab (inserirArvore esq True tb (No tab esq dir cima )) dir


