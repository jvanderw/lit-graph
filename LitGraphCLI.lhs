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
> import Control.Monad.IO.Class

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
>          print g
>          putStrLn "Menu Options"
>          putStr ("\t1 - Add character to graph\n"
>                 ++ "\t2 - Export to *.dot file\n"
>                 ++ "\t3 - Load graph from LitGraph generated *.dot file\n"
>                 ++ "\t4 - quit\n> ")
>          choice <- getLine
>          case choice of
>                      "1" -> addParams >>= \p ->
>                             menu (uncurry addCharacter p g)
>                      "2" -> export g >> menu g
>                      "3" -> importDot >>= \g2 -> menu g2
>                      "4" -> putStrLn "Exiting menu..."
>                      _   -> putStrLn "Not a valid menu option." >>
>                             menu g

--------------------------------------------------------------------------------

Functions that handle th various menu options available.

Get character and its list of connections.
FIXME: Validate the input - don't allow empty strings, or lists that don't
       contain valid node numbers

> addParams :: IO (Character, [Int])
> addParams = putStr "Enter character name: " >>
>             getLine >>= \name ->
>             putStr ("Characters " ++ name ++ " is connected to: ") >>
>             getLine >>= \connects ->
>             return (Character name, toList connects)

Convert the input from a string into [Int]

> toList   :: String -> [Int]
> toList s = read ("[" ++ s ++ "]")

Write the graph out to a *.dot file.
FIXME: Get input for more graph parameters like page size, rotation, etc.

> export   :: LitGraph -> IO ()
> export g = putStr "File name: " >>
>            getLine >>= \fname ->
>            writeFile fname $ graphvizUndir' (mkUndirGraph g)

Read in a graph from a given *.dot file.
FIXME: Need import functionallity in LitGraph module, currently this
       just returns an empty graph

> importDot :: IO LitGraph
> importDot = putStr "Enter name of file to import: " >>
>             getLine >>= \fname ->
>             putStrLn ("Opening " ++ fname) >>
>             readFile fname >>= \s ->
>             return (dot2LitGr s)