{-# LANGUAGE  NoMonomorphismRestriction #-}
{-
Algoritmo : C4.5
Autor     : Erick Fernandes da Cruz, 2018
Email     : erickfecruz@hotmail.com
-}

module C45Par where

import Control.Parallel.Strategies
import Data.List
import Data.Ord
import qualified Data.Map.Strict as M
import Data.List.Split (chunksOf)

import Vector
import Dados

data DecisionTree = Raiz Integer [(DecisionTree)]
              | No String Integer [(DecisionTree)]
              | Folha String
              deriving (Eq,Ord,Show,Read)

type Point = ([String], String)
type Dict = [[String]]

c45Par :: ChunksOf [Point] -> Dict -> DecisionTree
c45Par train dict = arvoreDecisao
  where
    arvoreDecisao = Raiz (atributoGanhoMaior train) (retornaListaArvoreRec (atributoGanhoMaior train) train dict)

parseFile :: String -> ([Point], Dict)
parseFile file = (dataset, dict)
  where
    dataset     = map parseLine (lines file)
    parseLine l = splitN (words l) 
    splitN  l   = (init l, last l)
    dict        = map nub $ transpose $ map fst dataset 

entropia :: Double -> Double -> Double 
entropia 0 _ = 0
entropia _ 0 = 0
entropia x y = ((x/y) * (logBase 2 (x/y)))

freqPar :: (NFData a, Ord a) => ChunksOf [a] -> [(a, Double)]
freqPar xs = mapReduceByKey (\x -> (x,1.0)) (+) xs

freqyPar :: ChunksOf [Point] -> M.Map String Double
freqyPar xys = M.fromList $ freqPar y
  where y = parmap (map snd) xys
  
entropiaClasse :: ChunksOf [Point] -> Double
entropiaClasse chunk = calcEntropia chunkClasse
  where 
    calcEntropia a = snd $ head $ combine (+) $ map (\(x,y) -> ("Chave",-(entropia y (fromIntegral $ head $ map length' chunk)))) a
    chunkClasse = M.toList $ freqyPar chunk

freqxyPar :: ChunksOf [Point] -> M.Map (Integer, String, String) Double
freqxyPar xys = M.fromList $ freqPar $ ixy
  where
    ixy  = parmap (\xy -> concat $ map enumerate xy) xys
    enumerate (xi, yi) = zip3 [0,1..] xi (repeat yi)

entropiaAtributos :: ChunksOf [Point] -> [((Integer,String), Double)]
entropiaAtributos chunk = mapInicial
  where
    mapInicial = combine (+) $ map (\((a,b,c),x) -> ((a,b),calcEntropiaAtr (a,b) x)) freq 
    calcEntropiaAtr ax bx = ((M.findWithDefault 0 ax (dictLenghtAtributos freq))/(fromIntegral $ head $ map length' chunk)) * (entropia bx (M.findWithDefault 0 ax (dictLenghtAtributos freq)))
    freq = M.toList $ freqxyPar chunk

dictLenghtAtributos :: [((Integer,String,String), Double)] -> M.Map (Integer, String) Double
dictLenghtAtributos frequencia = M.fromList $ lista
  where
    lista = combine (+) $ map (\((a,b,c),x) -> ((a,b),x)) frequencia 

ganhoPorAtributo :: ChunksOf [Point] -> [(Integer,Double)]
ganhoPorAtributo y = combine (+) $ map (\((a,b),x) -> (a,((entropiaClasse y) + x))) $ entropiaAtributos y
    
atributoGanhoMaior :: ChunksOf [Point] -> Integer
atributoGanhoMaior x = snd $ M.findMax $ M.fromList $ map (\(a,b) -> (b,a)) $ ganhoPorAtributo x 
  
retornaListaArvoreRec :: Integer -> ChunksOf [Point] -> Dict -> [DecisionTree]
retornaListaArvoreRec a b c =
  if (entropiaClasse b) == 0
    then [Folha (snd $ b!!1!!1)]
    else map (\x -> (No x (atributoGanhoMaior $ filtrarChunk b a x) (retornaListaArvoreRec (atributoGanhoMaior $ filtrarChunk b a x) (filtrarChunk b a x) c))) $ c!!(fromIntegral a)

filtrarChunk :: ChunksOf [Point] -> Integer -> String -> ChunksOf [Point]
filtrarChunk a c b = chunksOf 1000 $ (filter (\x -> b == (fst x)!!(fromIntegral c)) $ concat $ map (\x -> x) a)