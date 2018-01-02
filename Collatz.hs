import Data.List
import IC.Graphics

type Vertex = (Integer,Integer)
type State  = (Vertex,Integer,Colour)
type Line   = (Vertex,Vertex,Colour)
type Branch = [Line]
type ColSeq = [Integer]

collatzSeq :: Integer -> ColSeq -> ColSeq
-- pre : the given number is not null
-- note: although the collatzseq of "1" is valid, it's useless to compute it. 
collatzSeq n seq
 | n == 1    = 1:seq
 | otherwise = collatzSeq (nextCoeff n) (n:seq)  
 
nextCoeff :: Integer -> Integer
-- pre : the given number is not null. 
nextCoeff n 
 | n `mod` 2 == 0 = n`div`2
 | otherwise      = (3*n)+1

collatz :: [Integer] -> [ColSeq]
collatz []      = []
collatz (n:ns)  =  (collatzSeq n []):collatz ns


-- number is odd   : turn right by 25 degrees
-- number is right : turn left by 25 degrees (cause why not ?)
-- get the int closest to number/2, to create proportionality in branches

deviate :: Integer -> State -> State 
deviate val state@((x,y),theta,col)
 | val `mod` 2 == 0 = ((x+val,y+(val`div`2)),theta+25,blue)
 | otherwise        = ((x+val,y+(val`div`2)),theta-25,green)


traceOne :: ColSeq -> State -> Branch
traceOne [] _
 = []
traceOne seq@(x:xs) initState@(vertex,theta,col) 
 = (vertex,vertex2,col'):(traceOne xs newState) 
 where newState@(vertex2,_,col') = deviate x initState

trace :: [ColSeq] -> Branch
trace []
 = []
trace seqs@(x:xs)
 = (traceOne x initState)++(trace xs) 


testSeq   = collatzSeq 3 []
initState = ((0,0),90,red)
testCollatz = collatz [1..10]
