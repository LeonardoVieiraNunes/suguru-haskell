import Modulos.OperacoesMatriz (tabuleiroInicialOtimizado,verfExisteCelulaSemCandidatoTabuleiro)
import Modulos.Construtores ( Celula, tabuleiro, Tabuleiro, tamanhoTabuleiro, setCands, celula, initTabuleiro)
import Modulos.Pilha(funcaoTop)

main :: IO ()
main = do
    let a = [((1,1),tabuleiroInicialOtimizado)]
    print (funcaoTop a (1,1))
    -- print (verfExisteCelulaSemCandidatoTabuleiro tabuleiroInicialOtimizado )
