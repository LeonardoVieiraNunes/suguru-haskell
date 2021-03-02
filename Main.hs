import Modulos.OperacoesMatriz ( getPosAdjacentes, preencherCandidatos )
import Modulos.Construtores(Tabuleiro, Celula, tabuleiro, celula, setValue, initTabuleiro, tamanhoTabuleiro, setCands)

main :: IO ()
main = do
    print "Hello!"
    -- como funfa pra setar o valor no tabuleiro
    let a = setValue (1, 1) 99 initTabuleiro
        b = setValue (1, 2) (-1) a
        c = preencherCandidatos (1, 2) b 
    print a
    print b
    print c
    