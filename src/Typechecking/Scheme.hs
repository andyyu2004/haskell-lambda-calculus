module Typechecking.Scheme (Scheme(..), Ctx(..), instantiate) where

import qualified Data.Map as Map
import Typechecking.Types(Type,  Types(..), newTVar, TI)
import qualified Data.Set as Set

data Scheme = Scheme [String] Type

instantiate :: Scheme -> TI Type
instantiate (Scheme bound t) = do
  -- fresh type variable for every universally quantified variable
  instantiated <- mapM (const newTVar) bound
  -- generate substitution by zipping the variables with their instantiations
  let s = Map.fromList $ zip bound instantiated
  -- return the substitution applied to the type
  return $ substitute s t

generalize :: Ctx -> Type -> Scheme
generalize ctx t =
  let vars = Set.toList $ Set.difference (ftv t) (ftv ctx)
   in Scheme vars t

instance Types Scheme where
  ftv (Scheme bound t) = Set.difference (ftv t) $ Set.fromList bound

  substitute s (Scheme bound t) =
    let pruned = foldr Map.delete s bound in Scheme bound $ substitute pruned t


newtype Ctx = Ctx (Map.Map String Scheme)

instance Types Ctx where substitute s (Ctx ctx) = Ctx $ fmap (substitute s) ctx
