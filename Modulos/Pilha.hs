module Modulos.Pilha(separarCandidatos, setTabPilha) where
import Data.Array ((!))
import Modulos.Construtores(Tabuleiro,setCands)
import Modulos.OperacoesMatriz(getCand,getVal)

type Pilha = [Tabuleiro]

separarCandidatos :: Tabuleiro -> (Int,Int) -> [Tabuleiro]
separarCandidatos tb (x,y) =
    let (ch:cb) = getCand (tb!(x,y))
        t1 = setCands (x, y) [ch] tb
        t2 = setCands (x, y) cb tb
    in [t1,t2]

setTabPilha :: Pilha -> Tabuleiro -> (Int,Int) -> Pilha
setTabPilha p tb (x,y) | getVal (tb!(x,y)) /= -1 = tb:p
                       | otherwise = separarCandidatos tb (x,y)