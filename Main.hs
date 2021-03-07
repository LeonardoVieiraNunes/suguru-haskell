import Data.Array (Array, (!))
import Modulos.OperacoesMatriz ( getPosAdjacentes,getVal,proximaCoordenada,preencherValorCandidatosTabuleiro, isCandidato, getCand, getCelulaPos,otimizarTabuleiro, updateCandidatosTabuleiro,tabuleiroInicialOtimizado)
import Modulos.Construtores(Tabuleiro, Celula, tabuleiro, celula, setValue, initTabuleiro, tamanhoTabuleiro, setCands)
import Modulos.Pilha(setTabPilha)


main :: IO ()
main = do
    -- print tabuleiroInicialOtimizado
    let a = setTabPilha [] tabuleiroInicialOtimizado (1,1)
        b = setTabPilha a (last a) (1,2)
        c = setTabPilha b (last b) (1,3)
    print (length c)
    print c
    print "Hello!"