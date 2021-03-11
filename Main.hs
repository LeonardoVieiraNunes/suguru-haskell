<<<<<<< HEAD
import Data.Array (Array, (!))
import Modulos.OperacoesMatriz ( getPosAdjacentes,getGrupoEvalorCelulasTabuleiro,getVal,proximaCoordenada,preencherValorCandidatosTabuleiro, isCandidato, getCand, getCelulaPos,otimizarTabuleiro, updateCandidatosTabuleiro,tabuleiroInicialOtimizado, agruparDuplicatas)
import Modulos.OperacoesMatriz ( getPosAdjacentes,filterById,allDifferent,getGrupoEvalorCelulasTabuleiro,getVal,proximaCoordenada,preencherValorCandidatosTabuleiro, isCandidato, getCand, getCelulaPos,otimizarTabuleiro, updateCandidatosTabuleiro,tabuleiroInicialOtimizado, verfTabuleiroCompleto)
import Modulos.Construtores(Tabuleiro, Celula, tabuleiro, celula, setValue, initTabuleiro, tamanhoTabuleiro, setCands)
import Modulos.Pilha(setTabPilha, funcaoTop)

=======
import Modulos.OperacoesMatriz (tabuleiroInicialOtimizado)
import Modulos.Pilha(funcaoTop)
>>>>>>> e8373d551e8a5310a8c454f6b388dbaacf99ef16

main :: IO ()
main = do
    let a = [((1,1),tabuleiroInicialOtimizado)]
<<<<<<< HEAD
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
    --print (getGrupoEvalorCelulasTabuleiro tabuleiroInicialOtimizado)
    print (length (agruparDuplicatas tabuleiroInicialOtimizado (1,1)))
    -- print tabuleiroInicialOtimizado 
=======
>>>>>>> e8373d551e8a5310a8c454f6b388dbaacf99ef16
    print (funcaoTop a (1,1))
