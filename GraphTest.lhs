Copyright (c) 2012 Jess VanDerwalker. All rights reserved.

GraphTest.lhs

Testing for the Data.Graph module

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

> module GraphTest where

Since GraphvizDirected is being used, and defines Landscape and
Portrait, these need to be hidden when importing Data.Graph.Inductive.

> import Data.Graph.Inductive hiding (Landscape, Portrait)
> import Data.Graph.Inductive.Graph
> import GraphvizDirected

--------------------------------------------------------------------------------

Simple utility functions to get the values out of a triple.

> fstOfTrip           :: (a, b, c) -> a
> fstOfTrip (a, _, _) = a

> sndOfTrip         :: (a,b,c) -> b
> sndOfTrip (_,b,_) = b

> thrdOfTrip         :: (a,b,c) -> c
> thrdOfTrip (_,_,c) = c

--------------------------------------------------------------------------------

Static graph

> v1 = ("Count", 1, [3,4])
> v2 = ("Danglars", 3, [1,4])
> v3 = ("Calderrouse", 4, [1,3])
> nl = [v1, v2, v3]

> n1 = (1, "Count")
> n2 = (2, "Danglars")
> n3 = (3, "Calderrouse")

> e1 = (1,2, 1)
> e2 = (1,3,2)
> e3 = (2,1,3)
> e4 = (3,1,4)
> e5 = (2,3,5)

 getGraph       :: [LNode] -> [LEdge] -> Gr String Integer
 getGraph ns es = mkGraph ns es

> graph1 :: Gr String Int
> graph1 = mkGraph [n1,n2,n3] [e1,e2,e3,e4,e5]

 getDot :: Gr a b ->

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
> rmDupEdg (x:xs) = x : rmDupEdg (filter (\y -> not(eqEdg x y)) xs)

> mkUndirGraph   :: Gr a b -> Gr a b
> mkUndirGraph g = mkGraph (labNodes g) (rmDupEdg (labEdges g))

--------------------------------------------------------------------------------

Main routine to create *.dot files for the graphs we're playing with.

> main :: IO()
> main = do
>        writeFile "graph1.dot"
>        $ graphvizDir (mkUndirGraph graph1) "TestGraph" (8.5,11.0) (1,1) Landscape False

