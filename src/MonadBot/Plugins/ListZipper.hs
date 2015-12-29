module MonadBot.Plugins.ListZipper
    ( ListZipper
    , forward
    , back
    ) where

type ListZipper a  = ([a], [a])

forward :: ListZipper a -> Maybe (ListZipper a)
forward (ys, x : xs) = Just (x : ys, xs)
forward _           = Nothing

back :: ListZipper a -> Maybe (ListZipper a)
back (y : ys, xs) = Just (ys, y : xs)
back _            = Nothing
