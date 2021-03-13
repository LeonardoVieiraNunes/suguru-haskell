module Modulos.Pilha(separarCandidatos, setTabPilha, funcaoTop) where
import Data.Array ((!))
import Modulos.Construtores(Tabuleiro,setCands,setCand, initTabuleiro)
import Modulos.OperacoesMatriz(getCand,verfExisteCelulaSemCandidatoTabuleiro,verfTabuleiroCompleto,getGrupoEvalorCelulasTabuleiro,getVal,verfMesmoUnicoElementoAdjacenteTabuleiro,proximaCoordenada, allDifferent, otimizarTabuleiro, tabuleiroInicialOtimizado)

type Pilha = [((Int,Int), Tabuleiro)]

separarCandidatos :: Tabuleiro -> (Int,Int) -> [((Int,Int),Tabuleiro)]
separarCandidatos tb (x,y) =
    let (ch:cb) = getCand (tb!(x,y))
        t1 = setCand (x, y) ch tb
        t2 = setCands (x, y) cb tb
    in [((x,y),t1),((x,y),t2)]

setTabPilha :: Pilha -> Tabuleiro -> (Int,Int) -> Pilha
setTabPilha (hp:bp) tb (x,y) | getVal (tb!(x,y)) /= -1 = ((x,y),tb):bp
                             | otherwise = separarCandidatos tb (x,y) ++ bp

getCoordenada :: ((Int,Int),Tabuleiro) -> (Int,Int)
getCoordenada ((x,y),tb) = (x,y)

getTabuleiro :: ((Int,Int),Tabuleiro) -> Tabuleiro
getTabuleiro ((x,y), tab) = tab

funcaoTop :: Pilha -> (Int,Int) -> Pilha
funcaoTop p (x,y) =
    let erro1 = verfMesmoUnicoElementoAdjacenteTabuleiro(otimizarTabuleiro(getTabuleiro (head p)))
        existeCelulaSemCandidato = verfExisteCelulaSemCandidatoTabuleiro(otimizarTabuleiro(getTabuleiro (head p)))
        todosDiferentes = allDifferent(getGrupoEvalorCelulasTabuleiro(otimizarTabuleiro(getTabuleiro (head p))))
        estaCompleto = verfTabuleiroCompleto(getTabuleiro(head p))
        in if estaCompleto then
            [head p]
        else if not erro1 && todosDiferentes && not existeCelulaSemCandidato then
            let nPilha = setTabPilha p (otimizarTabuleiro (getTabuleiro (head p))) (x,y)
            in funcaoTop nPilha (proximaCoordenada (getCoordenada (head p)))
        else
            funcaoTop (tail p) (getCoordenada (head (tail p)))