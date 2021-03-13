import Modulos.OperacoesMatriz (tabuleiroInicialOtimizado)
import Modulos.Pilha(resolveSuguru)

main :: IO ()
main = do
    let a = [((1,1),tabuleiroInicialOtimizado)]
    print (resolveSuguru a (1,1))
