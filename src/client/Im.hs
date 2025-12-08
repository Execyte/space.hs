module Im (
  beginFlags,
  withWindowFlags,
  withWindowOpenFlags,
  module DearImGui
) where

import Control.Monad (when)
import Data.Text(Text)

import UnliftIO
import DearImGui
import DearImGui.Internal.Text qualified as ImText
import DearImGui.Raw qualified as ImRaw -- I'm raw!

beginFlags :: MonadIO m => Text -> ImGuiWindowFlags -> m Bool
beginFlags name flags = liftIO $ do
  ImText.withCString name $ \ptr ->
    ImRaw.begin ptr Nothing $ Just flags

withWindowFlags :: MonadUnliftIO m => Text -> ImGuiWindowFlags -> (Bool -> m a) -> m a
withWindowFlags name flags = bracket (beginFlags name flags) (const ImRaw.end)

withWindowOpenFlags :: MonadUnliftIO m => Text -> ImGuiWindowFlags -> m () -> m ()
withWindowOpenFlags name flags action = withWindowFlags name flags (`when` action)
