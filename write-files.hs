{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import qualified Data.ByteString.Lazy as BL
import Data.Aeson

main = BL.getContents >>= (walkM (fmap concat . mapM f) :: Pandoc -> IO Pandoc) . either error id .  eitherDecode'

f :: Block -> IO [Block]
f b@(CodeBlock (i,cs,a) code) | Just name <- lookup "file" a = do
    appendFile ("files/" ++ T.unpack name) (T.unpack code)
    appendFile ("files/" ++ T.unpack name) "\n\n"
    return []
f _ = return []
