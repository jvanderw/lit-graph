{-
Copyright (c) 1999-2007, Martin Erwig
All rights reserved.

Small alterations to support undirected graphs.
Copyright (c) 2012 Jess VanDerwalker

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
-}

-- | Simple graphviz output. Based on Data.Graph.Inductive.Graphviz,
-- but altered to handle undirected graphs
module GraphvizDirected(
    Orient(..),
    Directed(..),
    graphvizDir,
    graphvizDir',
    graphvizUndir'
) where

import Data.Graph.Inductive.Graph

data Orient = Portrait | Landscape deriving (Eq, Show)

o2s :: Orient -> String
o2s Portrait = "\trotate = \"0\"\n"
o2s Landscape = "\trotate = \"90\"\n"

type Directed = Bool

d2s       :: Directed -> String
d2s True  = "digraph "
d2s False = "graph "

edgeDir :: Directed -> String
edgeDir True = " -> "
edgeDir False = " -- "

-- | Formats a graph for use in graphviz.
graphvizDir :: (Graph g, Show a, Show b) =>    g a b   -- ^ The graph to format
					 -> String  -- ^ The title of the graph
					 -> (Double, Double)	-- ^ The size
								-- of the page
					 -> (Int, Int)	-- ^ The width and
							-- height of the page
							-- grid
					 -> Orient  -- ^ The orientation of
						    -- the graph.
                                         -> Directed -- ^ Is the graph directed
                                                     -- or not.
					 -> String

i2d :: Int -> Double
i2d = fromInteger . toInteger

graphvizDir g t (w, h) p@(pw', ph') o d =
    let n = labNodes g
	e = labEdges g
	ns = concatMap sn n
	es = concatMap se e
	sz w' h' = if o == Portrait then show w'++","++show h' else show h'++","++show w'
	ps = show w++","++show h
	(pw, ph) = if o == Portrait then p else (ph', pw')
	--gs = show ((w*(i2d pw))-m)++","++show ((h*(i2d ph))-m)
	gs = sz (w*(i2d pw)) (h*(i2d ph))
    in d2s d++t++" {\n"
	    ++"\tmargin = \"0\"\n"
	    ++"\tpage = \""++ps++"\"\n"
	    ++"\tsize = \""++gs++"\"\n"
	    ++o2s o
	    ++"\tratio = \"fill\"\n"
	    ++ns
	    ++es
	++"}"
    where sn (n, a) | sa == ""	= ""
		    | otherwise	= '\t':(show n ++ sa ++ "\n")
	    where sa = sl a
	  se (n1, n2, b) = '\t':(show n1 ++ (edgeDir d) ++ show n2 ++ sl b ++ "\n")

-- | Format a graph for graphviz with reasonable defaults: title of \"fgl\",
-- 8.5x11 pages, one page, landscape orientation, digraph
graphvizDir' :: (Graph g, Show a, Show b) => g a b -> String
graphvizDir' g = graphvizDir g "fgl" (8.5,11.0) (1,1) Landscape True

-- | Format a graph for graphviz with reasonable defaults: title of \"fgl\",
-- 8.5x11 pages, one page, landscape orientation, undirected graph
graphvizUndir' :: (Graph g, Show a, Show b) => g a b -> String
graphvizUndir' g = graphvizDir g "fgl" (8.5,11.0) (1,1) Landscape False

sq :: String -> String
sq s@[c]                     = s
sq ('"':s)  | last s == '"'  = init s
	    | otherwise	     = s
sq ('\'':s) | last s == '\'' = init s
	    | otherwise	     = s
sq s                         = s

sl :: (Show a) => a -> String
sl a =
    let l = sq (show a)
    in if (l /= "()") then (" [label = \""++l++"\"]") else ""
