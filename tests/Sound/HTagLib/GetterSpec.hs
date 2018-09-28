module Sound.HTagLib.GetterSpec (spec) where

import Sound.HTagLib
import Sound.HTagLib.Test.Util
import Test.Hspec

spec :: Spec
spec =
  describe "getters" $ do
    mapM_ (withFile $ const simpleGetter) fileList
    mapM_ (withFile specializedGetter)    fileList

simpleGetter :: AudioTags -> Expectation
simpleGetter tags = do
  let path = atFileName tags
  extracted <- getTags path (id <$> sampleGetter path)
  extracted `shouldMatchTags` tags

specializedGetter :: FileType -> AudioTags -> Expectation
specializedGetter t tags = do
  let path = atFileName tags
  extracted <- getTags' path t (id <$> sampleGetter path)
  extracted `shouldMatchTags` tags
