
{-# LANGUAGE TemplateHaskell #-}
module He.Annotation
  ( module He.Annotation
  , module He.Annotation.Internal
  ) where

import Control.Comonad.Trans.Store
import H.Common
import Language.Haskell.TH
import Text.Parsec.Applicative.Types

import He.Annotation.Internal

class Annotated a where
  annotationLens :: Lens a Ann

-- | Modify the given 'data' or 'newtype' declarations to have an extra value parameter
--   of type Ann at the end, provide instances of Annotated for the declared types, and
--   provide smart constructors with names starting with 'mk' that take all parameters
--   of the real constructor except the Ann, plus emptyAnn at the end. Constructors
--   declared as records will have a randomly-generated unique name for the Ann field.
annotate :: Q [Dec] -> Q [Dec]
annotate = annotateWhen $ const True

annotateExcept :: [String] -> Q [Dec] -> Q [Dec]
annotateExcept names = annotateWhen $ \dec -> case dec of
  DataD _ name _ _ _ -> not $ nameBase name `elem` names
  NewtypeD _ name _ _ _ -> not $ nameBase name `elem` names
  _ -> True

annotateWhen :: (Dec -> Bool) -> Q [Dec] -> Q [Dec]
annotateWhen pred = (>>= return . concat) . (>>= mapM (annotate1 pred))

annotate1 :: (Dec -> Bool) -> Dec -> Q [Dec]
annotate1 pred decl | not $ pred decl = return [decl]
annotate1 _ (DataD cxt name tvs cons dvs) = do
    cons'  <- mapM annotateCon cons
    let smarts = catMaybes $ map (fmap mkSmartCon . conName) cons
    inst   <- mkAnnInst name (length tvs) cons
    return $ inst : DataD cxt name tvs cons' dvs : smarts
annotate1 _ (NewtypeD cxt name tvs con dvs) = do
    con'  <- annotateCon con
    let smart = maybeToList $ fmap mkSmartCon $ conName con
    inst  <- mkAnnInst name (length tvs) [con]
    return $ inst : NewtypeD cxt name tvs con' dvs : smart
annotate1 _ d = return [d]

annotateCon :: Con -> Q Con
annotateCon (NormalC name tys) = return $ NormalC name $ (NotStrict, ConT ''Ann) : tys
annotateCon (RecC name tys) =
  RecC name . (: tys) . (, NotStrict, ConT ''Ann) <$> newName "_ann"
annotateCon c@(InfixC _ _ _) = return c
annotateCon (ForallC tvs cxt con) = ForallC tvs cxt <$> annotateCon con

mkSmartCon :: Name -> Dec
mkSmartCon name =
  ValD (VarP . mkName $ "mk" ++ nameBase name)
    (NormalB $ AppE (ConE name) $ VarE 'emptyAnn) []

conName :: Con -> Maybe Name
conName (NormalC name _) = Just name
conName (RecC name _) = Just name
conName (InfixC _ _ _) = Nothing
conName (ForallC _ _ con) = conName con

mkAnnInst :: Name -> Int -> [Con] -> Q Dec
mkAnnInst name n cons = do
  body <- mkRunLensBody cons
  ts   <- mapM (const $ newName "t") [1 .. n]
  let name' = foldl AppT (ConT name) $ map VarT ts
  return $ InstanceD [] (AppT (ConT ''Annotated) name')
    [ ValD (VarP $ mkName "annotationLens") (NormalB $ AppE (ConE 'Lens) body) [] ]

-- | Make the body of the runLens function, which has type "a -> Store a b".
mkRunLensBody :: [Con] -> Q Exp
mkRunLensBody cons = do
  name <- newName "x"
  LamE [VarP name] . CaseE (VarE name) <$> mapM mkRunLensMatch cons

mkRunLensMatch :: Con -> Q Match
mkRunLensMatch (NormalC name tys) = mkRunLensMatch' name $ length tys
mkRunLensMatch (RecC name tys) = mkRunLensMatch' name $ length tys
mkRunLensMatch (InfixC _ _ _) = return $ Match WildP (NormalB $ VarE 'undefined) []
mkRunLensMatch (ForallC _ _ con) = mkRunLensMatch con

mkRunLensMatch' :: Name -> Int -> Q Match
mkRunLensMatch' name n = do
  oldAnnName <- newName "oldAnn"
  newAnnName <- newName "newAnn"
  otherNames <- mapM (const $ newName "other") [1 .. n]
  let
  { updater =
      LamE [VarP newAnnName]
      $ foldl AppE (ConE name)
      $ map VarE
      $ newAnnName : otherNames
  }
  return
    $ Match (ConP name $ map VarP $ oldAnnName : otherNames)
      ( NormalB
      $ AppE (AppE (ConE 'StoreT) (AppE (ConE 'Identity) updater))
      $ VarE oldAnnName
      ) []

class SourcePosEffect f where
  getSourcePos :: f SourcePos

locate :: (Annotated a, SourcePosEffect m, Monad m) => m a -> m a
locate m = do
  pos <- getSourcePos
  ret <- m
  return $ (annSourcePos . annotationLens) ^= Just pos $ ret

