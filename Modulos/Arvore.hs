module Modulos.Arvore (Arvore, minhaArvore, getCima, tabArvore, inserirArvore) where
import Data.Array (Array, (!))
import Modulos.OperacoesMatriz ( getPosAdjacentes, preencherCandidatos, isCandidato, getCand, getCelulaPos )
import Modulos.Construtores(Tabuleiro, Celula, tabuleiro, celula, setValue, initTabuleiro, tamanhoTabuleiro, setCands)

data Arvore a = Null | No Int (Arvore a) (Arvore a) (Arvore a)
    deriving (Show,Eq)

minhaArvore :: Arvore a
minhaArvore = No 0 Null Null Null

getCima :: Arvore a -> Arvore a
getCima (No tb esq dir cima) = cima

tabArvore ::  Arvore a -> Int 
tabArvore (No tb esq dir cima) = tb

--arvore onde sera inserido(nulo caso seja folha) -> tabuleiro que sera inserido -> retorna arvore inicial
inserirArvore :: Arvore a -> Bool -> Int  -> Arvore a -> Arvore a
inserirArvore Null _ tb cima = No tb Null Null cima 
inserirArvore (No tab esq dir cima) menor tb pai 
            |tab == tb = No tab esq dir cima
            |not menor = No tab esq (inserirArvore dir False tb (No tab esq dir cima )) pai
            |otherwise = No tab (inserirArvore esq True tb (No tab esq dir cima )) dir pai


