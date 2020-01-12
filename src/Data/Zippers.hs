module Data.Zippers where

import Data.List (break)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)
data Direction = L | R deriving (Show)
type Directions = [Direction]
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show, Eq)
type Breadcrumbs a = [Crumb a]
type Zipper a = (Tree a, Breadcrumbs a)
x -: f = f x

changeOfP :: Directions -> Tree Char -> Tree Char
changeOfP (L:ds) (Node x l r) = Node x (changeOfP ds l) r
changeOfP (R:ds) (Node x l r) = Node x l (changeOfP ds r)
changeOfP [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree  a -> a
elemAt ds tr = x where (Node x _ _) = subTree ds tr

subTree :: Directions -> Tree  a -> Tree a
subTree (L:ds) (Node _ l _) = subTree ds l
subTree (R:ds) (Node _ _ r) = subTree ds r
subTree [] st = st

goLeft :: Zipper a -> Zipper a
goLeft (Node x l r, bs) = (l, LeftCrumb x r :bs)
goLeft (Empty, bs) = (Empty, bs)

goRight :: Zipper a -> Zipper a
goRight (Node x l r, bs) = (r, RightCrumb x l :bs)
goRight (Empty, bs) = (Empty, bs)

goUp :: Zipper a -> Zipper a
goUp (l, LeftCrumb x r :bs) = (Node x l r, bs)
goUp (r, RightCrumb x l :bs) = (Node x l r, bs)
goUp (tr, []) = (tr, [])

goTop :: Zipper a -> Zipper a
goTop (tr, []) = (tr, [])
goTop z = goTop . goUp $ z

modify :: (a -> a) -> Zipper a -> Zipper a
modify _ (Empty, bs) = (Empty, bs)
modify f (Node x l r, bs) = (Node (f x) l r, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach nt (tr, bs) = (nt, bs)

freeTree :: Tree Char
freeTree =
  Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
    )

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, []) = (item, [])
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in  (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

myDisk :: FSItem
myDisk =
    Folder "root"
        [ File "goat_yelling_like_man.wmv" "baaaaaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "dijon_poupon.doc" "best mustard"
        , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]


