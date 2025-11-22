module Client.ImUtils (
  beginFlags,
  withWindowFlags,
  withWindowOpenFlags
) where

import Control.Monad (when)
import Data.Text (Text)

import UnliftIO
import qualified DearImGui as Im
import qualified DearImGui.Internal.Text as ImText
import qualified DearImGui.Raw as ImRaw -- I'm raw!

beginFlags :: MonadIO m => Text -> Im.ImGuiWindowFlags -> m Bool
beginFlags name flags = liftIO $ do
  ImText.withCString name $ \ptr ->
    ImRaw.begin ptr Nothing $ Just flags

withWindowFlags :: MonadUnliftIO m => Text -> Im.ImGuiWindowFlags -> (Bool -> m a) -> m a
withWindowFlags name flags = bracket (beginFlags name flags) (const ImRaw.end)

withWindowOpenFlags :: MonadUnliftIO m => Text -> Im.ImGuiWindowFlags -> m () -> m ()
withWindowOpenFlags name flags action = withWindowFlags name flags (`when` action)