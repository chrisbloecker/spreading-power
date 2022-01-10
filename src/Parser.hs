{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Parser
    where
--------------------------------------------------------------------------------
import Data.Text             (Text)
import Data.Void             (Void)
import Model
import Text.Megaparsec
import Text.Megaparsec.Char
--------------------------------------------------------------------------------
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

--------------------------------------------------------------------------------

parseGraph :: Bool -> Parser Graph
parseGraph directed = do
    edges <- many (edge <* eol)
    eof
    let edges'     = if directed
                         then edges
                         else concat [ [(u,v), (v,u)] | (u,v) <- edges ]
        neighbours = foldr (\(u,v) m -> M.insertWith S.union u (S.singleton v) m) M.empty edges'

    return Graph{..}
    
    where
        edge :: Parser Edge
        edge = do u <- many alphaNumChar
                  choice [ char ' ', char ',' ]
                  v <- many alphaNumChar
                  return (u,v)