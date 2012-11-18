Copyright (c) 2012 Jess VanDerwalker. All rights reserved.

LitGraph.lhs

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Neither the name of the author nor the names of its contributors may be
   used to endorse or promote products derived from this software without
   specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

--------------------------------------------------------------------------------

> module LitGraph where

FIXME: Since GraphvizDirected is being used, and defines Landscape and
Portrait, these need to be hidden when importing
Data.Graph.Inductive. Perhaps there is a better way to handle this.

> import Data.Graph.Inductive hiding (Landscape, Portrait)
> import Data.Graph.Inductive.Graph
> import GraphvizDirected
> import Text.Regex.Posix hiding (empty, match)
> import Data.List
> import Data.Maybe

--------------------------------------------------------------------------------

Data types

A "Character" represents a character in a book or movie. These serve
as the nodes of the graph. Connections between characters are the
edges of the graph.

> data Character = Character { name :: String
>                         }
>                    deriving (Eq,Ord)

> instance Show Character where
>     show c = show (name c)

> type LitGraph = Gr Character ()

--------------------------------------------------------------------------------

Utility functions

> labUEdges :: [Edge] -> [UEdge]
> labUEdges = map (\(i,j) -> (i,j,()))

--------------------------------------------------------------------------------
Conversion functions for handling undirected graphs

For undirected graphs, edges such as (1,2) and (2,1) are
"equal". Currently assume that all egdes are LEdges, but that they
don't actually have meaningful labels. These edges need to be removed
before the graph is passed on to graphvizDir, otherwise there will be
mutiple edges between all the nodes in the graph.

> eqEdg :: LEdge a -> LEdge a -> Bool
> eqEdg (x,y,_) (m,n,_) | x == n && y == m = True
>                       | x == m && y == n = True
>                       | otherwise = False

> rmDupEdg    :: [LEdge a] -> [LEdge a]
> rmDupEdg []     = []
> rmDupEdg (x:xs) = x : rmDupEdg (filter (not . eqEdg x ) xs)

> mkUndirGraph   :: Gr a b -> Gr a b
> mkUndirGraph g = mkGraph (labNodes g) (rmDupEdg (labEdges g))

--------------------------------------------------------------------------------

Adding and removing characters from the graph.

Adding characters.

> addCharacter        :: Character 
>                        -> [Node]
>                        -> LitGraph
>                        -> LitGraph
> addCharacter c ns g = addEdges (n,c) ns (addNode c n g)
>     where n = head (newNodes 1 g)

> addNode       :: Character -> Node -> LitGraph -> LitGraph
> addNode c n = insNode (n,c)

Create connections going both to and from the new character and the
characters that they have been identified as being connected to.

> addEdges        :: LNode Character 
>                    -> [Node]
>                    -> LitGraph 
>                    -> LitGraph
> addEdges n ns = addEdges' ([(x,fst n, ()) | x <- ns]
>                            ++ [ (fst n,x,()) | x <- ns])

> addEdges'          :: [LEdge ()] -> LitGraph -> LitGraph
> addEdges' es g     = foldl (flip insEdge) g es

Removing characters from the graph

> rmCharByName      :: String -> LitGraph -> (Maybe (LNode Character), LitGraph)
> rmCharByName s g = let r = getNodeByChar s g in
>                        case r of
>                          Just (n,c) -> (Just (n,c), snd (match n g))
>                          Nothing    -> (Nothing, g)

> getNodeByChar     :: String -> LitGraph -> Maybe (LNode Character)
> getNodeByChar s g = find (\x -> isCharacterMatch s x) ns
>     where ns = labNodes g

> isCharacterMatch     :: String -> LNode Character -> Bool
> isCharacterMatch s n = name (snd n) == s

--------------------------------------------------------------------------------

Writing a out the LitGraph to a *.dot file.

> write :: LitGraph -> String
> write g = undefined

--------------------------------------------------------------------------------

Reading LitGraph from a LitGraph generated *.dot file.

First, given a list of strings which constitue the contents of a *.dot
file, pull out all the character nodes and add them to the graph.

> isNode   :: String -> Bool
> isNode s = s =~ "label"

> nodeNum   :: String -> Int
> nodeNum s = read (s =~ "[1-9][0-9]*")

> nodeLabel   :: String -> String
> nodeLabel s = filter (/= '"') (s =~ "\".*\"")

> allNodes        :: [String] -> [LNode Character]
> allNodes []     = []
> allNodes (x:xs) =   if isNode x
>                     then (nodeNum x, Character (nodeLabel x)) : allNodes xs
>                     else allNodes xs

> addDotNodes :: LitGraph -> [LNode Character] -> LitGraph
> addDotNodes = foldl (flip insNode)

Get the edges of the graph. We can use the existing addEdges function.

> isEdge   :: String -> Bool
> isEdge s = s =~ "[0-9]+ -- [0-9]+"

> edgeNode   :: String -> LEdge ()
> edgeNode s = (read (s =~ "[0-9]+"), read (s =~ "[0-9]+$"), ())

> allEdges        :: [String] -> [LEdge ()]
> allEdges []     = []
> allEdges (x:xs) = if isEdge x 
>                   then edgeNode x : allEdges xs
>                   else allEdges xs


Get the nodes and the edges, and return the graph. But, before
returning it, make sure that it is an undirected graph. Remember,
LitGraphs are undirected, but if there are multiple edges between
nodes in the *.dot file, then multiple egdes are rendered in the graph
- so they are removed before the *.dot file is generated. They need to
be put back when the graph is read in.

> dot2LitGr   :: String -> LitGraph
> dot2LitGr s = undir 
>               $ addEdges' (allEdges xs) $ addDotNodes empty $ allNodes xs
>     where xs = lines s

--------------------------------------------------------------------------------
Test graph

> n1 = (1, Character "Count")
> n2 = (2, Character "Danglars")
> n3 = (3, Character "Calderrouse")
> n4 = (4, Character "Morrel")

> e1 = (1,2)
> e2 = (1,3)
> e3 = (2,1)
> e4 = (3,1)
> e5 = (2,3)
> e6 = (3,2)
> e7 = (1,4)
> e8 = (4,1)

> graph1 :: LitGraph
> graph1 = mkGraph [n1,n2,n3,n4] (labUEdges [e1,e2,e3,e4,e5,e6,e7,e8])
