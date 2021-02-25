type IdGrupo = Int
type TamanhoGrupo = Int
type ValorCelula = Int

type Celula = (IdGrupo, TamanhoGrupo, ValorCelula)
type Tabuleiro = [[Celula]]

celula :: Int -> Int -> Celula
celula 1 1 = (1, 1, 1)
celula 1 2 = (2, 5, 5)
celula 1 3 = (3, 4, 2)
celula 1 4 = (3, 4, 1)

celula 2 1 = (2, 5, 2)
celula 2 2 = (2, 5, 3)
celula 2 3 = (2, 5, 4)
celula 2 4 = (3, 4, 3)

celula 3 1 = (4, 2, 1)
celula 3 2 = (2, 5, 5)
celula 3 3 = (5, 4, 1)
celula 3 4 = (3, 4, 2)

celula 4 1 = (4, 2, 2)
celula 4 2 = (5, 4, 4)
celula 4 3 = (5, 4, 3)
celula 4 4 = (5, 4, 4)


tabuleiro :: Tabuleiro
tabuleiro = [[celula 1 1, celula 1 2, celula 1 3, celula 1 4], [celula 2 1, celula 2 2, celula 2 3, celula 2 4], [celula 3 1, celula 3 2, celula 3 3, celula 3 4], [celula 4 1, celula 4 2, celula 4 3, celula 4 4]]

main =
    print tabuleiro
