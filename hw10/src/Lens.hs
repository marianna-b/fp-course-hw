{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Lens where

import           Control.Applicative    (Const (..), getConst)
import           Control.Lens           (Profunctor, lens, lmap, rmap, (%~), (&), (.~),
                                         (^.), (^?))
import           Control.Lens.Fold      (filtered, firstOf, folded, (^..))
import           Control.Lens.Prism     (Prism', prism')
import           Control.Lens.Traversal (Traversal', traversed)
import           Control.Lens.Type      (Lens')
import           Control.Monad.Identity (Identity (..), runIdentity)
import           Data.Foldable          (foldl')
import           Data.Maybe             (fromMaybe, isJust, isNothing)
import           System.Directory       (doesFileExist)
import           System.File.Tree       (Tree (..), getDirectory', toTree)
import           System.FilePath.Lens   (extension)

set  :: Lens' s a -> a -> s -> s
set l v obj = runIdentity $ l (const $ return v) obj

view :: Lens' s a -> s -> a
view l obj = getConst $ l Const obj

over :: Lens' s a -> (a -> a) -> s -> s
over l f obj = runIdentity $ l (return . f) obj

lenc :: (s -> a) -> (s -> a -> s) -> Lens' s a
lenc get set f s = set s <$> f (get s)

data FS = Dir  { name     :: FilePath
               , contents :: [FS]
               }
        | File { name     :: FilePath
               }
        deriving Show

nameFS :: Lens' FS FilePath
nameFS = lens name setName
  where setName (File _) n  = File n
        setName (Dir _ c) n = Dir n c

contentsFS :: Lens' FS [FS]
contentsFS = lens contents setCont
  where setCont (Dir n _) c = Dir n c
        setCont f _         = f

_File :: Prism' FS FS
_File = prism' id get
  where get (File n) = Just $ File n
        get _        = Nothing

_Dir :: Prism' FS FS
_Dir = prism' id get
  where get (Dir n c) = Just $ Dir n c
        get _         = Nothing

process :: FilePath -> Tree FilePath -> IO FS
process prefix t = case subForest t of
  [] -> do
    f <- doesFileExist (prefix ++ rootLabel t)
    if f then return $ File (rootLabel t) else retDir []
  xs -> retDir xs
  where retDir xs = Dir (rootLabel t) <$> traverse (process $ prefix ++ rootLabel t ++ "/") xs

getDir :: FilePath -> IO FS
getDir str = do
  dirs <- getDirectory' str
  let tree = toTree dirs
  process "" tree

subDir :: FS -> [FS]
subDir f = f^.contentsFS

fileNameMaybe :: FS -> Maybe FilePath
fileNameMaybe f = f^?_File.nameFS

fileName :: FS -> FilePath
fileName f = f^._File.nameFS

newRoot :: FilePath -> FS -> FS
newRoot n f = f & nameFS .~ n

firstDir :: FS -> Maybe FS
firstDir f =  firstOf (folded.filtered (\x -> isJust $ x^?_Dir)) (f^.contentsFS)

files :: FS -> [FS]
files f = f^.contentsFS^..folded.filtered (\x -> isJust $ x^?_File)

addPrefix :: FilePath -> FS -> FS
addPrefix pref f = f & nameFS %~ (pref++)

cd :: FilePath -> Lens' FS FS
cd s = lens getCd setCd
  where setCd (Dir n c) f = Dir n $ map (\x -> if isNothing (x^?_Dir) || (x^.nameFS /= s) then f else x) c
        setCd f _ = f
        getCd d@(Dir _ c) = fromMaybe d $ firstOf (folded.filtered (\x -> isJust (x^?_Dir) && (x^.nameFS == s))) c
        getCd f = f

ls :: Traversal' FS FilePath
ls = contentsFS.traversed.nameFS

file :: FilePath -> Prism' FS FilePath
file s = prism' File get
  where get (File n) = if n == s then Just n else Nothing
        get (Dir _ c) = name <$> res
          where res = firstOf (folded.filtered (\x -> isJust (x^?_File) && (x^.nameFS == s))) c


changeExtInDir :: FilePath -> FS -> FS
changeExtInDir s (Dir n c) = Dir n $ map (nameFS.extension .~ s) c
changeExtInDir _ f         = f

fileRec :: FS -> [FS]
fileRec (Dir _ c) = foldl' (\x y -> x ++ fileRec y) [] c
fileRec f         = [f]


deleteIfEmpty :: FilePath -> FS -> FS
deleteIfEmpty s (Dir n c) = Dir n $ c^..folded.filtered (\x -> isNothing (x^?_Dir) || (x^.nameFS /= s) || not (null $ x^.contentsFS))
deleteIfEmpty _ f = f


type MoveFS = (FilePath, FS)

move :: FilePath -> Lens' MoveFS MoveFS
move s = lens getMove setMove
  where getMove (a, b) = (a ++ "/" ++ s, b)
        setMove _ (x, y) = (x, y)

getPath :: Lens' MoveFS FilePath
getPath = lens getMove setMove
  where getMove (a, b) = b^.nameFS ++ a
        setMove (_, b) x = (x, b)

type Iso b a = forall p f. (Profunctor p, Functor f) => p a (f a) -> p b (f b)

iso :: (b -> a) -> (a -> b) -> Iso b a
iso ba ab = lmap ba . rmap (fmap ab)

newtype Tagged s b = Tagged { unTagged :: b }

retag :: Tagged s b -> Tagged t b
retag = Tagged . unTagged

instance Functor (Tagged s) where
    fmap f (Tagged x) = Tagged (f x)

instance Profunctor Tagged where
  lmap _ = retag
  rmap = fmap

--(p a (f a) -> p b (f b)) -> (p b (f b) -> p a (f a))
from :: Iso b a -> Iso a b
from i = iso ba ab
  where
    ba b = runIdentity . unTagged $ i (Tagged (Identity b))
    ab s = getConst (i Const s)

isoFS :: Iso FS (Tree FilePath)
isoFS = iso ba ab
  where ab (Node n []) = File n
        ab (Node n xs) = Dir n (map ab xs)
        ba b = Node (b^.nameFS) (map ba $ b^.contentsFS)
