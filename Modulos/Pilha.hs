module Modulos.Pilha(separarCandidatos, setTabPilha, funcaoTop) where
import Data.Array ((!))
import Modulos.Construtores(Tabuleiro,setCands,setCand)
import Modulos.OperacoesMatriz(getCand,getGrupoEvalorCelulasTabuleiro,getVal,updateCandidatosTabuleiro,verfMesmoUnicoElementoAdjacenteTabuleiro,preencheUnicosCandidatosTabuleiro,proximaCoordenada)

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

-- funcaoTop :: Pilha -> (Int,Int) -> (Int,Int) -> Pilha
-- funcaoTop (ph:pb) (x,y) origem =
--     -- verf a existência um unico candidato disponivel para celulas adjacentes, sinal que deu merda e passar pra proxima execução da pilha
--     -- se existe candidato vazio erro 2
--     if verfMesmoUnicoElementoAdjacenteTabuleiro (updateCandidatosTabuleiro ph) then
--             funcaoTop pb origem origem
--     else if (x,y) == (1,4) then
--             ph:pb
--     else
--         let tbLimpaTb = preencheUnicosCandidatosTabuleiro (updateCandidatosTabuleiro ph)
--             p2 = setTabPilha (ph:pb) tbLimpaTb (proximaCoordenada(x,y))
--         in funcaoTop p2 (proximaCoordenada(x,y)) (proximaCoordenada (x,y))

funcaoTop :: Pilha -> (Int,Int) -> Pilha
funcaoTop (ph:pb) (x,y) =
    let erro1 = verfMesmoUnicoElementoAdjacenteTabuleiro (updateCandidatosTabuleiro (getTabuleiro ph))

    in if erro1 then
            funcaoTop pb (getCoordenada ph)
        else if (x,y) == (4,4) then
            ph:pb
        else
            let nPilha = setTabPilha (ph:pb) (getTabuleiro ph) (proximaCoordenada (x,y))
            in funcaoTop nPilha (proximaCoordenada (x,y))