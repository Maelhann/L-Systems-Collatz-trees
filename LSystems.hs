module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper,
  dragon, snowflake, tree, bush :: System

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--  Functions for working with systems.

-- |Returns the rotation angle for the given system.
angle :: System -> Float
angle (angle,base,rules)
 = angle 

-- |Returns the base string for the given system.
base :: System -> String
base (angle,base,rules)
 = base

-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (angle,base,rules)
 = rules

-- |Look up a character in the given set of rules.
--
-- Pre: the character exists in the set of rules.

lookupChar :: Char -> Rules -> String
lookupChar c [] 
 = []
lookupChar c rules 
 = head [s|(x,s)<-rules , c == x] 

-- |Expand a command once using the given set of rules.
expandOne :: Rules -> String -> String
expandOne [] _ 
 = []
expandOne _ []
 = []
expandOne rules liste@(x:xs)  
 = lookupChar x rules ++ expandOne rules xs 

-- |Expand a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand rules key 0 
 = key 
expand rules key 1 
 = expandOne rules key
expand rules key reps 
 = expandOne rules (expand rules key (reps-1)  ) 

-- |Move a turtle.
--
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.

trace1 :: String -> Float -> Colour -> [ColouredLine]
trace1 = undefined 


move :: Char -> TurtleState -> Float -> TurtleState
move c state@((x,y),theta) a 
 | c == 'F' = (( x+cos v , y+sin v ),theta) 
 | c == 'L' =((x,y),(theta + a ))
 | c == 'R' =((x,y),(theta - a ))
 where v  =  theta / 180*pi
 

trace2 :: String -> Float -> Colour -> [ColouredLine]
trace2 commands@(x:xs) value col
 = map (helper2  col) (helper commands value ((0,0),90) [])
 
helper2 :: Colour  ->  (Vertex,Vertex,TurtleState) -> ColouredLine
helper2  col (c1,c2,state)  
 = (c1,c2, col)

helper :: String -> Float -> TurtleState -> [TurtleState] -> [(Vertex,Vertex,TurtleState)]
helper [] _ _ _
 = []
helper string@(c:cs) value state stack  
 | c == '['               =  helper cs value state (state:stack)
 | c == ']'               =  helper cs value (head stack) (tail stack) 
 | oldCoords == newCoords =  helper cs value currState stack
 | otherwise              = (oldCoords,newCoords,currState): helper cs value currState stack
 where currState@(newCoords,_)    = move c state value
       (oldCoords,_)              = state
      





--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

--  Some test systems.

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

drawLSystem1 :: System -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (lSystem system n) (angle system) colour)

drawLSystem2 :: System -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (lSystem system n) (angle system) colour)
