{-# LANGUAGE  NoMonomorphismRestriction #-}
{-
Algoritmo : C4.5
Autor     : Erick Fernandes da Cruz, 2018
Email     : erickfecruz@hotmail.com
-}

module C45 where

import Control.Parallel.Strategies
import Data.List
import Data.Ord
import qualified Data.Map.Strict as M
import Data.List.Split (chunksOf)

import Vector
import Dados

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> ([Point], Dict, [String])
parseFile file = (dataset, dict, klass)
  where
    dataset     = map parseLine (lines file)
    parseLine l = splitN (words l) 
    splitN  l   = (init l, last l)
    dict        = map nub $ transpose $ map fst dataset 
    klass       = nub $ map snd dataset

entropia :: Double -> Double -> Double 
entropia 0 _ = 0
entropia x y = ((x/y) * (logBase 2 (x/y)))

freqPar :: (NFData a, Ord a) => ChunksOf [a] -> [(a, Double)]
freqPar xs = mapReduceByKey (\x -> (x,1.0)) (+) xs

freqyPar :: ChunksOf [Point] -> M.Map String Double
freqyPar xys = M.fromList $ freqPar y
  where y = parmap (map snd) xys
  
entropiaClasse :: ChunksOf [Point] -> Double
entropiaClasse chunk = mapReduce (\(x,y) -> -(entropia y (length chunk))) $ freqyPar chunk

freqxyPar :: ChunksOf [Point] -> M.Map (Integer, String, String) Double
freqxyPar xys = M.fromList $ freqPar $ ixy
  where
    ixy  = parmap (\xy -> concat $ map enumerate xy) xys
    enumerate (xi, yi) = zip3 [0,1..] xi (repeat yi)

--Função ‘tamanho total de cada chave’ ainda não desenvolvida
entropiaAtributos :: ChunksOf [Point] -> M.Map Integer Double
entropiaAtributos x = mapReduceByKey(\((a,b),x) -> ((a),x))) (+) primeiraEtapa
  where
    primeiraEtapa = mapReduceByKey (\((a,b,c),x) -> ((a,b),	((‘tamanho total de cada chave’)/(length   dataset)) * entropia x (‘tamanho total de cada chave’))) (+) x

ganhoPorAtributo :: ChunksOf [Point] -> M.Map Integer Double
ganhoPorAtributo y = parMap(\a,x) -> (a,entropiaClasse - x) y

--Falta criar estrutura de arvore e aplicar as funções já criadas
