import Graph -- For Q2 (b) & (c)

--Q1

--(a)

data BST a = EmptyBST | N ( BST a ) a ( BST a ) deriving Show

--Useful for testing (b) and (c)
sampleBST :: BST Int
sampleBST = N ( N ( N EmptyBST 4 EmptyBST ) 2 ( N EmptyBST 5 EmptyBST ) ) 1 ( N ( N EmptyBST 6 EmptyBST ) 3 ( N EmptyBST 7 EmptyBST ) )

--(b)

sumBST :: BST Int -> Int
sumBST EmptyBST      = 0
sumBST (N left x right) = x + ( sumBST left ) + ( sumBST right )

--(c)

sameShape :: BST a -> BST b -> Bool
sameShape EmptyBST EmptyBST = True
sameShape EmptyBST _        = False
sameShape _        EmptyBST = False
sameShape (N left1 _ right1) (N left2 _ right2) = ( sameShape left1 left2 )
                                                 && ( sameShape right1 right2 )

--Q2

--(a)

--module ( Graph, Node, Edge,
--         nodes, edges, n2s, s2n, ns2e, e2ns,
--         insertEdge, insertNode, outEdges ) where

--data Graph = [ Node ] [ Edge ]
--data Node  = N String deriving Eq
--data Edge  = E Node Node deriving Eq

---- nodes g : the list of nodes in the graph 'g'
--nodes :: Graph -> [ Node ]

---- edges g : the list of edges in the graph 'g'
--edges :: Graph -> [ Edge ]

---- n2s n : the label of node 'n'
--n2s :: Node -> String

---- s2n s : the node with label 's'
--s2n :: String -> Node

---- ns2e ns : the edge with nodes from two-tuple ns
--ns2e :: ( Node, Node ) -> Edge

---- e2ns ns : the two-tuple of the start and finish nodes of edge 'e'
--e2ns :: Edge -> ( Node, Node )

---- insertEdge g e : the graph 'g' with the addition of edge 'e'
--insertEdge :: Graph -> Edge -> Graph

---- insertNode g n : the graph 'g' with the addition of node 'n'
--insertNode :: Graph -> Node -> Graph

---- outEdges n g : the list of edges adjacent to node 'n' in graph 'g'
--outEdges :: Node -> Graph -> [ Edges ]

--(b)

-- neighbours1 n g : the list of nodes in graph 'g' adjacent to node 'n'
neighbours1 :: Node -> Graph -> [ Node ]
neighbours1 n g = [ snd ( e2ns e ) | e <- ( outEdges n g ) ]

dfs :: Graph -> Node -> [ Node ]
dfs g s = dfs' g [ s ] [ ]

--dfs' g next seen : 
dfs' :: Graph -> [ Node ] -> [ Node ] -> [ Node ]
dfs' _ [ ] _      = [ ]
dfs' g ( n : ns ) seen
    | elem n seen = dfs' g ns seen
    | otherwise   = n : dfs' g ( ( neighbours1 n g ) ++ ns ) ( n : seen )

--(c)

isConnected :: Graph -> Bool
isConnected g = all (`elem`( dfs g n )) ns
    where ns = nodes g
          n  = head ns

--Q3

--(a)

-- Add this function and call it from eval'
-- apply'and  ( Sboolean b1 : Sboolean b2 : ss ) = Sboolean ( b1 && b2 ) : ss

--(b)

-- This is not lazy because both operands must be taken from the stack prior to the and operation.

--(c)

