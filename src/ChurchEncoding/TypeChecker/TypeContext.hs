module ChurchEncoding.TypeChecker.TypeContext where

import ChurchEncoding.TypeChecker.Type(Type(..))

import qualified Data.Map

type TypeContext = Data.Map.Map String Type

data WithTypeContext a = WTC (TypeContext -> (TypeContext, a))

instance Functor WithTypeContext where
  fmap f (WTC g) = WTC $ \ctx -> let (ctx',x) = g ctx in (ctx', f x)


instance Applicative WithTypeContext where
  pure x = WTC $ \ctx -> (ctx, x)
  (WTC f) <*> (WTC g) = WTC $ \ctx -> let (ctx',h) = f ctx
                                          (ctx'',x) = g ctx'
                                       in
                                          (ctx'', h x)

instance Monad WithTypeContext where
    WTC f >>= g = WTC $ \ctx -> let (ctx', x) = f ctx
                                    WTC gg = g x
                                 in gg ctx'
    return x = WTC $ \ctx -> (ctx, x)

runWithTypeContext :: WithTypeContext a -> TypeContext -> a
runWithTypeContext (WTC f) ctx = snd $ f ctx

getCtx :: WithTypeContext TypeContext
getCtx = WTC $ \ctx -> (ctx, ctx)

insertCtx :: String -> Type -> WithTypeContext ()
insertCtx x t = WTC $ \ctx -> (Data.Map.insert x t ctx, ())

withShadow :: String -> Type -> WithTypeContext a -> WithTypeContext a
withShadow x t (WTC f) = WTC $ \ctx -> let xOld = Data.Map.lookup x ctx
                                           (ctx',r) = f (Data.Map.insert x t ctx)
                                           ctx'' = case xOld of Nothing -> Data.Map.delete x ctx'; Just t' -> Data.Map.insert x t' ctx'
                                        in (ctx'', r)

freshTVar :: WithTypeContext Type
freshTVar = WTC $ \ctx -> let vars = [ x  | (_,TVar x) <- Data.Map.toList ctx ]
                              fresh = 1 + (foldr max 0 vars)
                           in (Data.Map.insert ("$$dummy" ++ show fresh) (TVar fresh) ctx, TVar fresh)
