module Modulos.OperacoesMatriz(getCand,verfExisteCelulaSemCandidatoTabuleiro,verfTabuleiroCompleto,getGrupoEvalorCelulasTabuleiro,getVal,verfMesmoUnicoElementoAdjacenteTabuleiro,proximaCoordenada, allDifferent, otimizarTabuleiro,tabuleiroInicialOtimizado) where
import Modulos.Construtores ( Celula, Tabuleiro, tamanhoTabuleiro, initTabuleiro, tamanhoGrupos)
import Data.List((\\))
import Data.Array (Array, array, (//), (!))

-- verifica se a coordenada está nos limites do tabuleiro
isInRange :: Tabuleiro -> Int -> Int -> Bool
isInRange t i j = i <= tamanhoTabuleiro t && j <= tamanhoTabuleiro t

-- getters de células vizinhas
getCelulaEsq :: Tabuleiro -> Int -> Int -> (Int, Int)
getCelulaEsq t i j | isInRange t i j && j-1 > 0 = (i, j-1)
                   | otherwise = (-1,-1)

getCelulaDir :: Tabuleiro -> Int -> Int -> (Int, Int)
getCelulaDir t i j | isInRange t i j && j < tamanhoTabuleiro t = (i, j+1)
                   | otherwise = (-1, -1)

getCelulaCima :: Tabuleiro -> Int -> Int -> (Int, Int)
getCelulaCima t i j | isInRange t i j && i-1 > 0 = (i-1, j)
                    | otherwise = (-1,-1)

getCelulaBaixo :: Tabuleiro -> Int -> Int -> (Int, Int)
getCelulaBaixo t i j | isInRange t i j && i < tamanhoTabuleiro t = (i+1, j)
                     | otherwise = (-1,-1)

getCelulaDiagEsqCima :: Tabuleiro -> Int -> Int -> (Int, Int)
getCelulaDiagEsqCima t i j | isInRange t i j && i-1 > 0 && j-1 > 0 = (i-1, j-1)
                           | otherwise = (-1,-1)

getCelulaDiagDirCima :: Tabuleiro -> Int -> Int -> (Int, Int)
getCelulaDiagDirCima t i j | isInRange t i j && i-1 > 0 && j < tamanhoTabuleiro t = (i-1, j+1)
                           | otherwise = (-1,-1)

getCelulaDiagEsqBaixo :: Tabuleiro -> Int -> Int -> (Int, Int)
getCelulaDiagEsqBaixo t i j | isInRange t i j && i < tamanhoTabuleiro t && j-1 > 0 = (i+1, j-1)
                            | otherwise = (-1,-1)

getCelulaDiagDirBaixo :: Tabuleiro -> Int -> Int -> (Int, Int)
getCelulaDiagDirBaixo t i j | isInRange t i j && i < tamanhoTabuleiro t && j < tamanhoTabuleiro t = (i+1, j+1)
                            | otherwise = (-1,-1)


-- utiliza getters acima para obter todas as células adjacentes
getPosAdjacentes :: Tabuleiro -> Int -> Int -> [(Int, Int)]
getPosAdjacentes t i j = filter (\c -> c /= (-1,-1)) [getCelulaDiagEsqCima t i j, getCelulaCima t i j, getCelulaDiagDirCima t i j, getCelulaEsq t i j, getCelulaDir t i j, getCelulaDiagEsqBaixo t i j, getCelulaBaixo t i j, getCelulaDiagDirBaixo t i j]

-- getter de valor
getVal :: Celula -> Int
getVal (id, val, cand) = val

-- getter de candidatos
getCand :: Celula -> [Int]
getCand (id, val, cand) = cand

-- retorna tupla com id do grupo e valor da célula
getGrupoEvalor :: Celula -> (Int,Int)
getGrupoEvalor (id,val,_) = (id,val)

-- retorna tupla com coordenada x e y da posição
getCelulaPos :: (Int ,Int) -> Tabuleiro -> Celula
getCelulaPos (x,y) tb = tb!(x,y)

-- preenche candidatos a partir do tamanho do grupo 
preencherValorCandidatosCelula :: Celula -> Celula
preencherValorCandidatosCelula (id,val,cand) | val == -1 = (id,val,[1..tamanhoGrupos id])
                                             | otherwise = (id,val,cand)

-- chama a função acima para todo o tabuleiro
preencherValorCandidatosTabuleiro :: Tabuleiro -> Tabuleiro
preencherValorCandidatosTabuleiro tb = tb // [((x,y),preencherValorCandidatosCelula(tb!(x,y))) | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]]

-- getters de valor e candidatos de células adjacentes
getValorAdjacentes :: (Int,Int) -> Tabuleiro -> [Int]
getValorAdjacentes (x,y) tb = filter (\x-> x /= -1) [ getVal(getCelulaPos co tb) | co <- getPosAdjacentes tb x y]

getCandAdjacentes :: (Int,Int) -> Tabuleiro -> [[Int]]
getCandAdjacentes (x,y) tb = filter (/= []) [getCand(getCelulaPos co tb) | co <- getPosAdjacentes tb x y]

-- atualiza candidatos das células removendo valores que já se encontram no grupo ou em células adjacentes
updateCandidatosCelula :: (Int,Int) -> Tabuleiro -> Celula
updateCandidatosCelula (x,y) tb =
    let (id,val,cand) = getCelulaPos(x,y) tb
    in (id,val, (cand \\ getValorAdjacentes(x,y) tb) \\ getValorGrupoById id tb)

-- chama a função acima para todo o tabuleiro
updateCandidatosTabuleiro :: Tabuleiro -> Tabuleiro
updateCandidatosTabuleiro tb = tb // [((x,y),updateCandidatosCelula(x,y) tb) | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]]

-- caso a célula dada tenha apenas um candidato, esse candidato se torna o valor da célula
insereValorCelulaUmCandidato :: Celula -> Celula
insereValorCelulaUmCandidato (id,val,cand) | val == -1 && length cand == 1 = (id,head cand,[])
                                           | otherwise = (id,val,cand)

-- chama a função acima para todo o tabuleiro
preencheUnicosCandidatosTabuleiro :: Tabuleiro -> Tabuleiro
preencheUnicosCandidatosTabuleiro tb = tb // [((x,y),insereValorCelulaUmCandidato(tb!(x,y))) | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]]

-- otimiza o tabuleiro recursivamente, preenchendo valores que podem ser deduzidos sem necessidade de backtracking
otimizarTabuleiro :: Tabuleiro -> Tabuleiro
otimizarTabuleiro tb | tb == preencheUnicosCandidatosTabuleiro tb = tb
                     | otherwise = otimizarTabuleiro(updateCandidatosTabuleiro(preencheUnicosCandidatosTabuleiro tb))

-- prepara o tabuleiro inicial com otimizações, antes de começar o processo de resolução por backtracking
tabuleiroInicialOtimizado :: Tabuleiro
tabuleiroInicialOtimizado =
    let a = preencherValorCandidatosTabuleiro initTabuleiro
        b = updateCandidatosTabuleiro a
    in otimizarTabuleiro b

-- atualiza coordenada para que não saia dos limites da matriz
proximaCoordenada :: (Int,Int) -> (Int,Int)
proximaCoordenada (x,y) | y < tamanhoTabuleiro initTabuleiro = (x,y+1)
                        | otherwise = (x+1,1)

-- verifica situação em que alguma das células adjacentes à célula dada não tem valor e tem apenas um mesmo candidato, causando erro
verfMesmoUnicoCandAdjacenteCelula :: (Int,Int) -> Tabuleiro -> Bool
verfMesmoUnicoCandAdjacenteCelula (x,y) tb = length (getCand (tb!(x,y))) == 1 && getCand (tb!(x,y)) `elem` getCandAdjacentes (x,y) tb

-- aplica a função acima ao tabuleiro inteiro
verfMesmoUnicoElementoAdjacenteTabuleiro :: Tabuleiro -> Bool
verfMesmoUnicoElementoAdjacenteTabuleiro tb = or ([verfMesmoUnicoCandAdjacenteCelula (x,y) tb | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]])

-- verifica se uma célula dada não tem valores candidatos para se inserir, causando erro 
verfCelulaSemCandidato :: (Int,Int) -> Tabuleiro -> Bool
verfCelulaSemCandidato (x,y) tb = getVal (tb!(x,y)) == -1 && null (getCand(tb!(x,y)))

-- aplica a função acima no tabuleiro inteiro
verfExisteCelulaSemCandidatoTabuleiro :: Tabuleiro -> Bool
verfExisteCelulaSemCandidatoTabuleiro tb = or ([verfCelulaSemCandidato (x,y) tb | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]])

-- verifica se o jogo está completo, avaliando se todas as células estão preenchidas
verfTabuleiroCompleto :: Tabuleiro -> Bool
verfTabuleiroCompleto tb = and [getVal(tb!(x,y)) /= -1 | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]]

-- retorna tuplas com os grupos do tabuleiro e os valores que estes hrupos contém
getGrupoEvalorCelulasTabuleiro :: Tabuleiro -> [(Int, Int)]
getGrupoEvalorCelulasTabuleiro tb = filter (\(id,val) -> val /= -1) [getGrupoEvalor(tb!(x,y)) | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]]

-- retorna, na forma de tupla os valores de um grupo dado
filterById :: Int -> Tabuleiro -> [(Int,Int)]
filterById id tb = filter (\(i,val) -> i==id) [getGrupoEvalor(tb!(x,y)) | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]]

-- retira da tupla retornada pela função acima apenas os valores das células
getValorGrupoById :: Int -> Tabuleiro -> [Int]
getValorGrupoById id tb = map snd (filterById id tb)

-- verifica se todos os elementos de uma lista são diferentes, usada na pilha
allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs