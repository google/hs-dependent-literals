-- Copyright 2020-2021 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | A GHC plugin making numeric literals and patterns pseudo-dependently-typed.

module DependentLiterals.Plugin (plugin) where

import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust)

import qualified Data.Generics as SYB

-- GHC has been overhauling its module hierarchy, but to a significant extent,
-- it's renaming modules entirely and not regrouping exports; most of these we
-- paper over by defining macros for their most-up-to-date name with
-- underscores instead of dots.

-- This one was renamed twice in the range of supported versions.
#if MIN_VERSION_ghc(9,0,0)
#define GHC_Hs_Type GHC.Hs.Type
#elif MIN_VERSION_ghc(8,10,0)
#define GHC_Hs_Type GHC.Hs.Types
#else
#define GHC_Hs_Type HsTypes
#endif

-- Renames from 8.10 to 9.0
#if MIN_VERSION_ghc(9,0,0)
#define GHC_Plugins GHC.Plugins
#define GHC_Types_SourceText GHC.Types.Basic
#define GHC_Types_Name_Occurrence GHC.Types.Name.Occurrence
#define GHC_Types_Name_Reader GHC.Types.Name.Reader
#define GHC_Unit_Module_Name GHC.Unit.Module.Name
#define GHC_Utils_Outputable GHC.Utils.Outputable
#else
#define GHC_Plugins GhcPlugins
#define GHC_Types_SourceText BasicTypes
#define GHC_Types_Name_Occurrence OccName
#define GHC_Types_Name_Reader RdrName
#define GHC_Unit_Module_Name Module
#define GHC_Utils_Outputable Outputable
#endif

-- Renames from 8.8 to 8.10
#if MIN_VERSION_ghc(8,10,0)
#define GHC_Hs GHC.Hs
#define GHC_Hs_Expr GHC.Hs.Expr
#define GHC_Hs_Extension GHC.Hs.Extension
#define GHC_Hs_Lit GHC.Hs.Lit
#define GHC_Hs_Pat GHC.Hs.Pat
#define GHC_Hs_Utils GHC.Hs.Utils
#else
#define GHC_Hs HsSyn
#define GHC_Hs_Expr HsExpr
#define GHC_Hs_Extension HsExtension
#define GHC_Hs_Lit HsLit
#define GHC_Hs_Pat HsPat
#define GHC_Hs_Utils HsUtils
#endif

import GHC_Hs
         ( HsModule(..), HsWildCardBndrs(HsWC)
         , HsTyLit(HsNumTy)
         , ImportDecl(..), IEWrappedName(..), IE(..)
         )
import GHC_Hs_Lit (HsOverLit(..), OverLitVal(HsIntegral))
import GHC_Hs_Expr (HsExpr(HsAppType, HsOverLit, HsApp, NegApp), LHsExpr)
import GHC_Hs_Extension (GhcPs, GhcPass)
import GHC_Hs_Type
         ( HsType(HsAppTy, HsParTy, HsTyLit, HsTyVar)
         , LHsType, HsConDetails(PrefixCon)
         )
import GHC_Hs_Pat (LPat, Pat(NPat, ViewPat))
import GHC_Hs_Utils (nlHsVar, nlHsApp)
import GHC_Plugins
         ( Hsc, HsParsedModule(..)
         , Plugin(parsedResultAction, pluginRecompile), defaultPlugin
         , PluginRecompile(NoForceRecompile)
         , CommandLineOption
         , DynFlags, Located, GenLocated(L), noSrcSpan
         , getDynFlags, liftIO
         , gopt_set, GeneralFlag(Opt_SuppressModulePrefixes)
         , SrcSpan
         )
import GHC_Types_Name_Occurrence (OccName, mkTcOcc, mkVarOcc, mkDataOcc)
import GHC_Types_Name_Reader (RdrName, mkRdrQual, mkRdrUnqual)
import GHC_Types_SourceText (IntegralLit(IL), SourceText(NoSourceText))
import GHC_Unit_Module_Name (ModuleName, mkModuleName)
import GHC_Utils_Outputable
         ( (<+>), Outputable, nest, pprPrec, sep, showSDoc, text
         )

-- For semantic changes, we generally try to paper over them by adding
-- compatibility shims, e.g. pattern synonyms, to make the code below look like
-- it's targeting the most-up-to-date version.

#if MIN_VERSION_ghc(9,0,0)
import GHC_Hs_Pat (Pat(ConPat))
import GHC.Unit.Types (IsBootInterface(..))
#else
-- Imports for pre-9.0 compatibily shims.
import GHC_Hs_Pat (Pat(ConPatIn), HsConPatDetails)
import GHC_Hs_Extension (IdP)
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC.Hs.Extension (NoExtField(..))
import GHC.Hs.ImpExp (ImportDeclQualifiedStyle(..))
import GHC_Plugins (noLoc)
#else
-- Imports for pre-8.10 compatibily shims.
import HsExtension (NoExt(..))
#endif

#if MIN_VERSION_ghc(8,8,0)
import GHC_Types_SourceText (PromotionFlag(..))
#else
-- Imports for pre-8.8 compatibily shims.
import GHC_Plugins (noLoc)
import HsTypes (Promoted(..))
#endif

-- Pre-9.0 compatibility shims.
#if !MIN_VERSION_ghc(9,0,0)
pattern ConPat :: a -> Located (IdP p) -> HsConPatDetails p -> Pat p
pattern ConPat ext con args <- (((,) NoExtField) -> (ext, ConPatIn con args))
 where
  ConPat _ext con args = ConPatIn con args

type HsModulePs = HsModule GhcPs

pattern NotBoot :: Bool
pattern NotBoot = False
#else
type HsModulePs = HsModule
#endif

-- Pre-8.10 compatibility shims.
#if !MIN_VERSION_ghc(8,10,0)
type NoExtField = NoExt
pattern NoExtField :: NoExtField
pattern NoExtField = NoExt

type ImportDeclQualifiedStyle = Bool
pattern QualifiedPre, NotQualified :: ImportDeclQualifiedStyle
pattern QualifiedPre = True
pattern NotQualified = False
#endif

-- Pre-8.8 compatibility shims.
#if !MIN_VERSION_ghc(8,8,0)
type PromotionFlag = Promoted
pattern IsPromoted :: PromotionFlag
pattern IsPromoted = Promoted
#endif

data Config = Config
  { _cDoLiterals :: Bool
  , _cDoPatterns :: Bool
  , _cTraceThings :: Bool
  }

defaultConfig :: Config
defaultConfig = Config True True False

interpretOpts :: [CommandLineOption] -> Config
interpretOpts opts0 = go opts0 defaultConfig
 where
  go [] c = c
  go ("nolits":opts) c = go opts c { _cDoLiterals = False }
  go ("nopats":opts) c = go opts c { _cDoPatterns = False }
  go ("trace":opts) c = go opts c { _cTraceThings = True }
  go (opt:_) _ = error $
    "Illegal option " ++ show opt ++
    ".\nAll options: " ++ show opts0

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = \opts _ -> parsedResultPlugin (interpretOpts opts)
  , pluginRecompile = \_ -> return NoForceRecompile
  }

parsedResultPlugin :: Config -> HsParsedModule -> Hsc HsParsedModule
parsedResultPlugin cfg m = do
  df <- getDynFlags
  hpm_module' <- transformParsed cfg df (hpm_module m)
  return $ m { hpm_module = hpm_module' }

when_ :: Applicative f => Bool -> (a -> f a) -> a -> f a
when_ True f = f
when_ False _ = pure

pattern LPat :: Pat (GhcPass p) -> LPat (GhcPass p)
#if !MIN_VERSION_ghc(8,8,0) || MIN_VERSION_ghc(8,10,0)
pattern LPat pat <- L _ pat
#else
pattern LPat pat <- pat
#endif

nlPat :: Pat (GhcPass p) -> LPat (GhcPass p)
nlPat = id
#if !MIN_VERSION_ghc(8,8,0) || MIN_VERSION_ghc(8,10,0)
  . noLoc
#endif

transformParsed
  :: Config
  -> DynFlags
  -> Located HsModulePs
  -> Hsc (Located HsModulePs)
transformParsed Config{..} df' (L modLoc HsModule{..}) = do
  decls <-
    pure hsmodDecls
      >>= when_ _cDoLiterals
            ( SYB.everywhereM (SYB.mkM (wrapDebug "expression" transformExp))
            . SYB.everywhere (SYB.mkT foldNegation)
            )
      >>= when_ _cDoPatterns
            (SYB.everywhereM (SYB.mkM (wrapDebug "pattern" transformPat)))

  return $ L modLoc $ HsModule
    { hsmodDecls = decls
    , hsmodImports =
        mkModImport litMod Nothing QualifiedPre Nothing :
        unqualLitModImport :
        qualIntModImport :
        hsmodImports
    , ..
    }
 where
  df = gopt_set df' Opt_SuppressModulePrefixes

  nl :: a -> Located a
  nl = L noSrcSpan

  litMod, intMod :: ModuleName
  litMod = mkModuleName "DependentLiterals.Int"
  intMod = mkModuleName "Kinds.Integer"

  -- import qualified DependentLiterals.Int
  mkModImport nm as q imports = nl $ ImportDecl
    NoExtField
    NoSourceText
    (nl nm)
    Nothing -- no package qualifier
    NotBoot
    False -- not marked safe
    q -- qualified
    True -- implicit
    as -- "as" rename
    ((False,) . nl . map nl <$> imports) -- no "hiding"

  -- Import a few things unqualified implicitly.  This way, when they appear in
  -- error messages, they won't have bulky module names attached.  All of these
  -- have -XMagicHash names so that they can't conflict with the subset of the
  -- namespace used by reasonable Haskell programmers, and most people can't
  -- ever tell that they're imported.
  -- TODO can we move this plugin post-renamer and do this by generating names
  -- that claim to have been originally unqualified?
  importVar = IEVar NoExtField . nl . IEName .nl
  importAll = IEThingAll NoExtField . nl . IEName . nl
  importTyOp = IEThingAbs NoExtField . nl . IEType . nl
  unqualLitModImport = mkModImport litMod Nothing NotQualified $ Just
    [ importVar litHashName
    , importTyOp minusHashName
    ]
  qualIntModImport = mkModImport intMod Nothing QualifiedPre $ Just
    [ importAll integerName
    ]

  qual :: OccName -> RdrName
  qual = mkRdrQual litMod

  integerName   = mkRdrUnqual (mkTcOcc "Integer")
  minusHashName = mkRdrUnqual (mkTcOcc "-#")
  negName       = mkRdrQual intMod (mkDataOcc "Neg")
  posName       = mkRdrQual intMod (mkDataOcc "Pos")
  cjustConName  = qual (mkDataOcc "CJust")
  litHashName   = mkRdrUnqual (mkVarOcc "lit#")
  matchHashName = qual (mkVarOcc "match#")

  infixl 4 `mkHsAppType`
  mkHsAppType :: LHsExpr GhcPs -> LHsType GhcPs -> HsExpr GhcPs
  mkHsAppType expr ty = HsAppType
#if MIN_VERSION_ghc(8,8,0)
    NoExtField
    expr
    (HsWC NoExtField ty)
#else
    (HsWC NoExtField ty)
    expr
#endif

  infixl 4 `nlHsAppType`
  nlHsAppType :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
  nlHsAppType expr ty = L noSrcSpan $ mkHsAppType expr ty

  infixl 4 `nlHsApp_`
  nlHsApp_ = nlHsApp

  litToTyLit :: IntegralLit -> LHsType GhcPs
  litToTyLit (IL txt neg val) = nl $ HsParTy NoExtField $ nl $ HsAppTy NoExtField
    (nl $ HsTyVar NoExtField IsPromoted $ nl (if neg then negName else posName))
    (nl $ HsTyLit NoExtField (HsNumTy txt (abs val)))

  debug :: String -> Hsc ()
  debug s
    | _cTraceThings = liftIO (putStrLn s)
    | otherwise = return ()

  wrapDebug :: Outputable a => String -> (a -> Maybe a) -> a -> Hsc a
  wrapDebug thing f x = do
    let r = f x
    for_ r (\x' ->
      debug $ showSDoc df $ sep
        [ text "Rewrote" <+> text thing <+> pprPrec 11 x <+> text "to"
        , nest 2 $ pprPrec 11 x'
        ])
    return $ fromMaybe x r

  extractLit :: HsExpr GhcPs -> Maybe (IntegralLit, HsExpr GhcPs)
  extractLit (HsOverLit _ (OverLit _ (HsIntegral il) w)) = Just (il, w)
  extractLit _ = Nothing

  fuseNegation :: Bool -> IntegralLit -> IntegralLit
  fuseNegation negated (IL _txt neg val) =
    let -- You can write patterns/exprs that are the negation of a neg literal.
        -- We'll just sweep those under the rug by making them into a positive
        -- literal.  If there's more than one negation, too bad.  Those will
        -- try to call into a Num instance.
        neg' = neg /= negated

        -- If the thing described in the previous comment happened, we have
        -- e.g. "-4" as the source text.  Just drop the source text always.
        txt' = NoSourceText

        -- Set the sign of the resulting literal according to 'neg''.
        val' = (if neg' then negate else id) (abs val)

        -- Refabricated literal.
    in  IL txt' neg' val'

  buildReprLit :: SrcSpan -> IntegralLit -> HsExpr GhcPs -> LHsExpr GhcPs
  buildReprLit l il witness =
    L l $ HsOverLit NoExtField $ OverLit NoExtField (HsIntegral il) witness

  rewriteLit :: SrcSpan -> Bool -> IntegralLit -> HsExpr GhcPs -> LHsExpr GhcPs
  rewriteLit l negated il witness =
    let il' = fuseNegation negated il
        wrapper = nlHsVar litHashName `nlHsAppType` litToTyLit il'
        lit = buildReprLit l il' witness
    in  L l $ HsApp NoExtField (nlHsApp wrapper lit) lit

  foldNegation :: LHsExpr GhcPs -> LHsExpr GhcPs
  foldNegation (L l (NegApp _ (L _ (extractLit -> Just (il, witness))) _)) =
    buildReprLit l (fuseNegation True il) witness
  foldNegation e = e

  transformExp :: LHsExpr GhcPs -> Maybe (LHsExpr GhcPs)
  transformExp (L l (extractLit -> Just (lit, witness))) =
    Just $ rewriteLit l False lit witness
  transformExp _ = Nothing

  transformPat :: LPat GhcPs -> Maybe (LPat GhcPs)
  transformPat (LPat (NPat _ (L l (OverLit _ (HsIntegral il) witness)) negation _)) =
    let il' = fuseNegation (isJust negation) il

        -- Wrapper application of match# to the LitRepr.
        wrappedLit =
          nlHsVar matchHashName
            `nlHsAppType` litToTyLit il'
            `nlHsApp_` buildReprLit l il' witness
            `nlHsApp_` buildReprLit l il' witness

    in  Just $ nlPat $ ViewPat NoExtField
            wrappedLit
            (nlPat $ ConPat NoExtField (nl cjustConName) (PrefixCon []))
  transformPat _ = Nothing
