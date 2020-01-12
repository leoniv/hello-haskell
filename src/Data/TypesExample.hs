module Data.TypesExample where

data Book = Book Cover [Page]
data Cover = SuperCover | SoftCover
data Page = Page String

instance Show Book where
  show (Book cover pages) = unwords . map show $ pages

instance Show Page where
  show = content

pageCount :: Book -> Int
pageCount (Book cover pages) = length pages

pageContent :: Book -> Int -> String
pageContent book page = content . pageGet book $ page

pageGet :: Book -> Int -> Page
pageGet (Book cover pages) page = pages !! (page - 1)

content :: Page -> String
content (Page text) = text

