import Data.Array (Array, (!))
import Modulos.OperacoesMatriz ( getPosAdjacentes, preencherCandidatos, isCandidato, getCand, getCelulaPos )
import Modulos.Construtores(Tabuleiro, Celula, tabuleiro, celula, setValue, initTabuleiro, tamanhoTabuleiro, setCands)
import Modulos.Arvore (Arvore, minhaArvore, getCima, tabArvore, inserirArvore)

-- inserirArvore :: (Ord a) => Arvore a -> Bool -> Tabuleiro -> Arvore a -> Arvore a

main :: IO ()
main = do
    print "Hello!"
    -- como funfa pra setar o valor no tabuleiro
    let a = tabArvore minhaArvore
        b = inserirArvore minhaArvore  False 5 minhaArvore 
        c = inserirArvore b True 2 b
        d= inserirArvore c True 4  c
        
        
        -- b = setValue (1, 2) 1 a
        -- c = setValue (1, 2) 3 a
        -- arvore_1 = inserirArvore minhaArvore True b minhaArvore 
        -- arvore_2 = inserirArvore minhaArvore False  c arvore_1 
        -- d= getCima arvore_2
        

        
        
    print minhaArvore
    print b
    print c
    print d
    
    
