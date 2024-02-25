-- @Author: Cristi Cretan
-- @Date:   01-05-2020 18:17:42
-- @Last Modified by:   Cristi Cretan
-- @Last Modified time: 02-05-2020 12:45:30

data Node = Node {
val :: Int,
neighb :: [Int]
}

node1 = Node 1 [2, 3]
node2 = Node 2 [1, 4]
node3 = Node 3 [1, 4]
node4 = Node 4 [2, 3]

bfs :: Node -> [([Node], [Node])]
bfs root = 







                         starea principala
                /      /         |          \         \           \ 
             copil1   copil2    copil3      copil4    copil5    copil6
                .....                ....                    ...
 