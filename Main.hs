import Modulos.OperacoesMatriz (tabuleiroInicialOtimizado,agruparDuplicatas,verfExisteCelulaSemCandidatoTabuleiro,allDifferent,getGrupoEvalorCelulasTabuleiro, otimizarTabuleiro)
import Modulos.Construtores ( Celula, tabuleiro, Tabuleiro, tamanhoTabuleiro, setCands, celula, initTabuleiro)
import Modulos.Pilha(funcaoTop)

main :: IO ()
main = do
    let a = [((1,1),tabuleiroInicialOtimizado)]
    print (funcaoTop a (1,1))
    -- let b = getGrupoEvalorCelulasTabuleiro(otimizarTabuleiro tabuleiroInicialOtimizado)
    --     c = allDifferent b
    -- print c
    -- print b
    -- print (verfExisteCelulaSemCandidatoTabuleiro tabuleiroInicialOtimizado )
