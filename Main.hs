import Data.Array (Array, (!))
import Modulos.OperacoesMatriz ( getPosAdjacentes, preencherCandidatos, isCandidato, getCand, getCelulaPos )
import Modulos.Construtores(Tabuleiro, Celula, tabuleiro, celula, setValue, initTabuleiro, tamanhoTabuleiro, setCands)
--import Modulos.Arvore (Arvore)

data Arvore a = Null | No Tabuleiro   [(Arvore a, Bool)]
    deriving (Show,Eq)

minhaArvore :: Arvore a
minhaArvore = No initTabuleiro [(Null,False )]

tabArvore :: Arvore a -> Tabuleiro
tabArvore (No tb filhos) = tb
--arvore onde sera inserido(nulo caso seja folha) -> tabuleiro que sera inserido -> retorna arvore inicial
inserirArvore :: Arvore a -> Tabuleiro -> Arvore a
inserirArvore Null tb = No tb [(Null,False)]
inserirArvore (No tab [(arv,visitado)]) tb = No tab [(inserirArvore arv tb,False)]

main :: IO ()
main = do
    print "Hello!"
    -- como funfa pra setar o valor no tabuleiro
    let a = tabArvore minhaArvore
        b = setValue (1, 2) 1 a
         
        novaArvore = inserirArvore minhaArvore b 
        d= setValue (1, 2) 3 a
        outraArvore = inserirArvore novaArvore d
        dd= setValue (2, 2) 1 d
        maisUmaArvore = inserirArvore outraArvore dd

        
        
    --print a
    --print aa
    --print c
    print minhaArvore
    print novaArvore
    print outraArvore
    print maisUmaArvore
    
    
    
