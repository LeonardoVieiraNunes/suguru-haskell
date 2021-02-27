module Modulos.OperacoesMatriz(getCand, getPosAdjacentes) where
import Modulos.Construtores ( Celula, Candidatos, tabuleiro )

getCand :: Celula -> Candidatos
getCand (id, val, cand) = cand

getCelulaEsq :: Int -> Int -> (Int, Int)
getCelulaEsq i j | i-1 > 0 = (i-1, j)
                 | otherwise = (-1,-1)

getCelulaDir :: Int -> Int -> (Int, Int)
getCelulaDir i j | i < length tabuleiro - 1 = (i+1, j)
                 | otherwise = (-1, -1)

getCelulaCima :: Int -> Int -> (Int, Int)
getCelulaCima i j | j-1 > 0 = (i, j-1)
                  | otherwise = (-1,-1)

getCelulaBaixo :: Int -> Int -> (Int, Int)
getCelulaBaixo i j | j < length tabuleiro - 1 = (i, j+1)
                   | otherwise = (-1,-1)

getCelulaDiagEsqCima :: Int -> Int -> (Int, Int)
getCelulaDiagEsqCima i j | i-1 > 0 && j-1 > 0 = (i-1, j-1)
                         | otherwise = (-1,-1)

getCelulaDiagDirCima :: Int -> Int -> (Int, Int)
getCelulaDiagDirCima i j | i < length tabuleiro - 1 && j-1 > 0 = (i+1, j+1)
                         | otherwise = (-1,-1)

getCelulaDiagEsqBaixo :: Int -> Int -> (Int, Int)
getCelulaDiagEsqBaixo i j | i-1 > 0 && j < length tabuleiro - 1 = (i-1, j+1)
                          | otherwise = (-1,-1)

getCelulaDiagDirBaixo :: Int -> Int -> (Int, Int)
getCelulaDiagDirBaixo i j | i < length tabuleiro - 1 && j < length tabuleiro - 1 = (i+1, j+1)
                          | otherwise = (-1,-1)

getPosAdjacentes :: Int -> Int -> [(Int, Int)]
getPosAdjacentes i j = [getCelulaDiagEsqCima i j, getCelulaCima i j, getCelulaDiagDirCima i j, getCelulaEsq i j, getCelulaDir i j, getCelulaDiagEsqBaixo i j, getCelulaBaixo i j, getCelulaDiagDirBaixo i j]