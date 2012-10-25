Copyright (c) 2012 Jess VanDerwalker. All rights reserved.

LitGraphIO.lhs

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

The LitGraphCLI module provides a command line interface for creating
and querying LitGraphs.

--------------------------------------------------------------------------------

> module LitGraphCLI where

> import LitGraph
> import GraphvizDirected
> import Data.Graph.Inductive

--------------------------------------------------------------------------------

The main function runs, prints a "hello" message and then calls the
"menu" function - which does all the actual interaction with the
user. Passes in an empty graph that can be built up by the user.

> main :: IO ()
> main = do
>        putStrLn "---# Welcome to LitGraph #---"
>        menu empty
>        putStrLn "--- Exiting LitGraph... ---"


The menu function gets the selection from the user and then calls the
correct function to handle the choice. Calls itself recursively until
the user decides to quit.

FIXME: Over time, the recursive calls would eat up memory,
so need to determine if there is a way to implement something close to
a 'while' loop.

> menu :: LitGraph -> IO ()
> menu g = do
>          putStrLn "Current character graph components:"
>          putStrLn (show g)
>          putStrLn "Menu Options"
>          putStr "\t1 - Add character to graph\n\t2 - Export to *.dot file\n\t3 - quit\n"
>          choice <- getLine
>          if choice == "1"
>          then do
>               putStr "Enter character name: "
>               name <- getLine
>               putStr ("Characters " ++ name ++ " is connected to: ")
>               connections <- getLine
>               putStrLn "Adding to graph..."
>               menu (addCharacter (Character name) (toList connections) g)
>          else if choice == "2"
>               then do
>                    putStr "File name: "
>                    fileName <- getLine
>                    writeFile fileName $ graphvizUndir' (mkUndirGraph g)
>          else putStrLn "Exiting menu..."

Convert the input from a string into [Int]

> toList :: String -> [Int]
> toList s = read ("[" ++ s ++ "]")
