Copyright (c) 2012 Jess VanDerwalker. All rights reserved.

LitGraph.lhs

Create and display the graph of connections between different
characters.

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

> module LitGraph where

FIXME: Since GraphvizDirected is being used, and defines Landscape and
Portrait, these need to be hidden when importing
Data.Graph.Inductive. Perhaps there is a better way to handle this.

> import Data.Graph.Inductive hiding (Landscape, Portrait)
> import Data.Graph.Inductive.Graph
> import GraphvizDirected

--------------------------------------------------------------------------------

Data types

> data Character = Character { name :: String
>                            }
>                  deriving (Show, Eq)

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
> rmDupEdg (x:xs) = x : rmDupEdg (filter (\y -> not(eqEdg x y)) xs)

> mkUndirGraph   :: Gr a b -> Gr a b
> mkUndirGraph g = mkGraph (labNodes g) (rmDupEdg (labEdges g))

--------------------------------------------------------------------------------

Adding and removing characters from the graph

> addCharacter        :: Character 
>                        -> [Node]
>                        -> Gr Character ()
>                        -> Gr Character ()
> addCharacter c ns g = addEdges (n,c) ns (addNode c n g)
>     where n = head (newNodes 1 g)

> addNode       :: Character -> Node -> Gr Character () -> Gr Character ()
> addNode c n g = insNode (n,c) g

Create connections going both to and from the new character and the
characters that they have been identified as being connected to.

> addEdges        :: LNode Character 
>                    -> [Node]
>                    -> Gr Character () 
>                    -> Gr Character ()
> addEdges n ns g = addEdges' ([(x,fst n, ()) | x <- ns]
>                              ++ [ (fst n,x,()) | x <- ns]) g

> addEdges'          :: [LEdge ()] -> Gr Character () -> Gr Character ()
> addEdges' [] g     = g
> addEdges' (e:es) g = addEdges' es (insEdge e g) 

--------------------------------------------------------------------------------

Test graph

> n1 = (1, Character "Count")
> n2 = (2, Character "Danglars")
> n3 = (3, Character "Calderrouse")

> e1 = (1,2)
> e2 = (1,3)
> e3 = (2,1)
> e4 = (3,1)
> e5 = (2,3)

> graph1 :: Gr Character ()
> graph1 = mkGraph [n1,n2,n3] (labUEdges [e1,e2,e3,e4,e5])
