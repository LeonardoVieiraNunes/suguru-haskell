module Modulos.OperacoesMatriz(getCand, getPosAdjacentes, preencherCandidatos, isCandidato, getCelulaPos) where
import Modulos.Construtores ( Celula, Valor, Candidatos, tabuleiro, Tabuleiro, tamanhoTabuleiro, setCands, celula )
import Data.List(intersect) 
import Data.Array (Array, array, (//), (!))
getCand :: Celula -> Candidatos
getCand (id, val, cand) = cand

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

-- funcoes novas

getVal :: Celula -> Valor
getVal (id, val, cand) = val


preencherCandidatos :: (Int, Int) -> Tabuleiro -> Tabuleiro
preencherCandidatos (x,y) tb | getVal (celula(x,y)) == -1 =  setCands (x,y) [1,2,3,4,5] tb
                             | otherwise = tb

isCandidato :: Celula  -> Int -> Bool 
isCandidato c z | intersect (getCand(c))  [z] == [] = False  
                   |otherwise = True  

getCelulaPos :: (Int ,Int ) -> Tabuleiro -> Celula 
getCelulaPos (x,y) tb = tb!(x,y)
