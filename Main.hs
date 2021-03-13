import Modulos.OperacoesMatriz (tabuleiroInicialOtimizado)
import Modulos.Pilha(funcaoTop)

main :: IO ()
main = do
    let a = [((1,1),tabuleiroInicialOtimizado)]
    print (funcaoTop a (1,1))
