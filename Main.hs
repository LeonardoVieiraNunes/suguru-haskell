import Data.Array (Array, (!))
import Modulos.OperacoesMatriz ( getPosAdjacentes, preencherCandidatos, isCandidato, getCand, getCelulaPos )
import Modulos.Construtores(Tabuleiro, Celula, tabuleiro, celula, setValue, initTabuleiro, tamanhoTabuleiro, setCands)

main :: IO ()
main = do
    print "Hello!"
    -- como funfa pra setar o valor no tabuleiro
    let a = setValue (1, 1) 99 initTabuleiro
        b = setValue (1, 2) (-1) a
        c = preencherCandidatos (1, 2) b 
        f = getCelulaPos  (1, 2) c
        e = isCandidato f 2
    --print a
    --print b
    print c
    print e
    
    
