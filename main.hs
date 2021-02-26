type IdGrupo = Int
type Valor = Int
type Candidatos = [Int]
type Celula = (IdGrupo, Valor, Candidatos)
type Tabuleiro = [[Celula]]

celula :: Int -> Int -> Celula
celula 1 1 = (1,1,[])
celula 1 2 = (1,-1,[1,2,3,4,5])

tabuleiro :: Tabuleiro
tabuleiro = [
    [celula 1 1, celula 1 2]
    ]