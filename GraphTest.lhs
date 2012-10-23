Copyright (c) 2012 Jess VanDerwalker. All rights reserved.

GraphTest.lhs

Testing for the Data.Graph module

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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

Make a graph out of the nodes and edges and send it to the graphviz
library to get a representation of it.

> main :: IO()
> main = do
>        writeFile "graph1.dot"
>        $ graphvizDir (mkUndirGraph graph1) "TestGraph" (8.5,11.0) (1,1) Landscape False

