{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- 
-- License     : Copyright
-- Maintainer  : Sergey Bushnyak, sergey.bushnyak@sigrlami.eu
-- Stability   : experimental
-- Portability : GHC
-- 
-- Entry point for site publishing

import           Control.Applicative
import           Control.Error hiding (left)
import           Control.Monad
import           Control.Monad.Trans
import           Data.Monoid
import           Data.Maybe
import           Data.String.Utils (replace)
import           Data.Time
import           Data.Time.Format
import           Hakyll
import           System.Locale
import           System.FilePath (normalise, takeBaseName, takeFileName)
import           Text.JSON
import           Text.Printf
import           Data.List (isPrefixOf, intercalate)
import           Text.Pandoc.Options
import           Prelude hiding (div, span)
import qualified Data.Map as Map
import qualified Data.Text as T

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do

    -- handle css files
    match "css/*" $ do
      route idRoute
      compile $ compressCssCompiler

    -- handle templates
    match "tpl/**" $ compile templateCompiler

    -- handle static files
    sequence_ $ fmap matchStatic
      [ "img/**", "js/*"]

    -- Create index
    create ["index.html"] $ do
      route idRoute
      compile $ do
        let tplM = fromFilePath ("tpl/index.tpl")
            -- tplD = fromFilePath ("tpl/default-index.tpl")
        makeItem ""
          >>= loadAndApplyTemplate tplM defaultContext 
          -- >>= loadAndApplyTemplate tplD defaultCtx
          >>= relativizeUrls

matchStatic :: Pattern -> Rules ()
matchStatic pattern = do
  match pattern $ do
    route idRoute
    compile copyFileCompiler
