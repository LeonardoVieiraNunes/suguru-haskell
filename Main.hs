import Data.Array (Array, (!))
import Modulos.OperacoesMatriz ( getPosAdjacentes )
import Modulos.Construtores(Tabuleiro, Celula, tabuleiro, celula, setValue, initTabuleiro, tamanhoTabuleiro, setValue, preencherValorCandidatosCelula, preencherValorCandidatosTabuleiro)

main = do
    print "Hello!"
    
    print (preencherValorCandidatosTabuleiro initTabuleiro )
    -- print (b!(1,1))
    -- print (b!(1,2))
    