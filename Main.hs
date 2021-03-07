import Data.Array (Array, (!))
import Modulos.OperacoesMatriz ( getPosAdjacentes,getGrupoEvalorCelulasTabuleiro,getVal,proximaCoordenada,preencherValorCandidatosTabuleiro, isCandidato, getCand, getCelulaPos,otimizarTabuleiro, updateCandidatosTabuleiro,tabuleiroInicialOtimizado)
import Modulos.Construtores(Tabuleiro, Celula, tabuleiro, celula, setValue, initTabuleiro, tamanhoTabuleiro, setCands)
import Modulos.Pilha(setTabPilha, funcaoTop)


main :: IO ()
main = do
    -- print tabuleiroInicialOtimizado
    let a = [((1,1),tabuleiroInicialOtimizado)]
    --     b = setTabPilha a (head a) (1,2)
    --     c = setTabPilha b (head b) (1,3)
    --     d = setTabPilha c (head c) (1,4)
    --     e = setTabPilha d (head d) (2,1)
    --     f = setTabPilha e (head e) (2,2)
    --     g = setTabPilha f (head f) (2,3)
    --     h = setTabPilha g (head g) (2,4)
    --     i = setTabPilha h (head h) (3,1)
    -- print (length c)
    -- print i
    -- print (funcaoTop a (1,1) (1,1))
    -- print(funcaoTop a (1,1))
    print (getGrupoEvalorCelulasTabuleiro tabuleiroInicialOtimizado)
    print "Hello!"