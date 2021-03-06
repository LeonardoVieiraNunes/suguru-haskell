module Modulos.Arvore (Arvore) where
import Data.Array (Array, (!))
import Modulos.OperacoesMatriz ( getPosAdjacentes, preencherCandidatos, isCandidato, getCand, getCelulaPos )
import Modulos.Construtores(Tabuleiro, Celula, tabuleiro, celula, setValue, initTabuleiro, tamanhoTabuleiro, setCands)


minhaArvore :: Arvore a
minhaArvore = No initTabuleiro Null Null

tabArvore :: Arvore a -> Tabuleiro
tabArvore (No tb esq dir) = tb
--arvore onde sera inserido(nulo caso seja folha) -> tabuleiro que sera inserido -> retorna arvore inicial
inserirArvore :: Arvore a -> Tabuleiro -> Arvore a
inserirArvore Null tb = No tb Null Null
inserirArvore (No tab esq dir) tb 
    |       No tab [(inserirArvore arv tb,False)]
    |
    |