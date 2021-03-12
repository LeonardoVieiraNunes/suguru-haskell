module Modulos.OperacoesMatriz(getCand,verfTabuleiroCompleto,verfExisteCelulaSemCandidatoTabuleiro,allDifferent,getGrupoEvalorCelulasTabuleiro,getVal,verfMesmoUnicoElementoAdjacenteTabuleiro,proximaCoordenada, otimizarTabuleiro, tabuleiroInicialOtimizado) where
import Modulos.Construtores ( Celula, tabuleiro, Tabuleiro, tamanhoTabuleiro, setCands, celula, initTabuleiro)
import Data.List((\\))
import Data.Array (Array, array, (//), (!))

tamanhoGrupo :: Int -> Int
tamanhoGrupo 1 = 6
tamanhoGrupo 2 = 4
tamanhoGrupo 3 = 5
tamanhoGrupo 4 = 6
tamanhoGrupo 5 = 5
tamanhoGrupo 6 = 5
tamanhoGrupo 7 = 3
tamanhoGrupo 8 = 5
tamanhoGrupo 9 = 5
tamanhoGrupo 10 = 4
tamanhoGrupo 11 = 1

isInRange :: Tabuleiro -> Int -> Int -> Bool
isInRange t i j = i <= tamanhoTabuleiro t && j <= tamanhoTabuleiro t

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

getPosAdjacentes :: Tabuleiro -> Int -> Int -> [(Int, Int)]
getPosAdjacentes t i j = filter (\c -> c /= (-1,-1)) [getCelulaDiagEsqCima t i j, getCelulaCima t i j, getCelulaDiagDirCima t i j, getCelulaEsq t i j, getCelulaDir t i j, getCelulaDiagEsqBaixo t i j, getCelulaBaixo t i j, getCelulaDiagDirBaixo t i j]

getVal :: Celula -> Int
getVal (id, val, cand) = val

getCand :: Celula -> [Int]
getCand (id, val, cand) = cand

getGrupoEvalor :: Celula -> (Int,Int)
getGrupoEvalor (id,val,_) = (id,val)

getCelulaPos :: (Int ,Int) -> Tabuleiro -> Celula
getCelulaPos (x,y) tb = tb!(x,y)

preencherValorCandidatosCelula :: Celula -> Celula
preencherValorCandidatosCelula (id,val,cand) | val == -1 = (id,val,[1..tamanhoGrupo id])
                                             | otherwise = (id,val,cand)

preencherValorCandidatosTabuleiro :: Tabuleiro -> Tabuleiro
preencherValorCandidatosTabuleiro tb = tb // [((x,y),preencherValorCandidatosCelula(tb!(x,y))) | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]]

getValorAdjacentes :: (Int,Int) -> Tabuleiro -> [Int]
getValorAdjacentes (x,y) tb = filter (\x-> x /= -1) [ getVal(getCelulaPos co tb) | co <- getPosAdjacentes tb x y]

getCandAdjacentes :: (Int,Int) -> Tabuleiro -> [[Int]]
getCandAdjacentes (x,y) tb = filter (/= []) [getCand(getCelulaPos co tb) | co <- getPosAdjacentes tb x y]

updateCandidatosCelula :: (Int,Int) -> Tabuleiro -> Celula
updateCandidatosCelula (x,y) tb =
    let (id,val,cand) = getCelulaPos(x,y) tb
    in (id,val, (cand \\ getValorAdjacentes(x,y) tb) \\ getValorGrupoById id tb)

updateCandidatosTabuleiro :: Tabuleiro -> Tabuleiro
updateCandidatosTabuleiro tb = tb // [((x,y),updateCandidatosCelula(x,y) tb) | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]]

insereValorCelulaUmCandidato :: Celula -> Celula
insereValorCelulaUmCandidato (id,val,cand) | val == -1 && length cand == 1 = (id,head cand,[])
                                           | otherwise = (id,val,cand)

preencheUnicosCandidatosTabuleiro :: Tabuleiro -> Tabuleiro
preencheUnicosCandidatosTabuleiro tb = tb // [((x,y),insereValorCelulaUmCandidato(tb!(x,y))) | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]]

otimizarTabuleiro :: Tabuleiro -> Tabuleiro
otimizarTabuleiro tb | tb == preencheUnicosCandidatosTabuleiro tb = tb
                     | otherwise = otimizarTabuleiro(updateCandidatosTabuleiro(preencheUnicosCandidatosTabuleiro tb))

tabuleiroInicialOtimizado :: Tabuleiro
tabuleiroInicialOtimizado =
    let a = preencherValorCandidatosTabuleiro initTabuleiro
        b = updateCandidatosTabuleiro a
    in otimizarTabuleiro b

proximaCoordenada :: (Int,Int) -> (Int,Int)
proximaCoordenada (x,y) | y < 7 = (x,y+1)
                        | otherwise = (x+1,1)

verfMesmoUnicoCandAdjacenteCelula :: (Int,Int) -> Tabuleiro -> Bool
verfMesmoUnicoCandAdjacenteCelula (x,y) tb = length (getCand (tb!(x,y))) == 1 && getCand (tb!(x,y)) `elem` getCandAdjacentes (x,y) tb

verfMesmoUnicoElementoAdjacenteTabuleiro :: Tabuleiro -> Bool
verfMesmoUnicoElementoAdjacenteTabuleiro tb = or ([verfMesmoUnicoCandAdjacenteCelula (x,y) tb | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]])

verfCelulaSemCandidato :: (Int,Int) -> Tabuleiro -> Bool
verfCelulaSemCandidato (x,y) tb = getVal (tb!(x,y)) == -1 && null (getCand(tb!(x,y)))

verfExisteCelulaSemCandidatoTabuleiro :: Tabuleiro -> Bool
verfExisteCelulaSemCandidatoTabuleiro tb = or ([verfCelulaSemCandidato (x,y) tb | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]])

verfTabuleiroCompleto :: Tabuleiro -> Bool
verfTabuleiroCompleto tb = and [getVal(tb!(x,y)) /= -1 | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]]

getGrupoEvalorCelulasTabuleiro :: Tabuleiro -> [(Int, Int)]
getGrupoEvalorCelulasTabuleiro tb = filter (\(id,val) -> val /= -1) [getGrupoEvalor(tb!(x,y)) | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]]

filterById :: Int -> Tabuleiro -> [(Int,Int)]
filterById id tb = filter (\(i,val) -> i==id) [getGrupoEvalor(tb!(x,y)) | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]]

getValorGrupoById :: Int -> Tabuleiro -> [Int]
getValorGrupoById id tb = map snd (filterById id tb)

allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

agruparDuplicatas :: Tabuleiro -> (Int ,Int) -> [(Int,Int)]
agruparDuplicatas tb (x,y) = filter (\(id,val) -> val /= -1 && (id,val) == getGrupoEvalor(tb!(x,y))) [getGrupoEvalor(tb!(x,y)) | x<-[1..tamanhoTabuleiro tb], y<-[1..tamanhoTabuleiro tb]]
