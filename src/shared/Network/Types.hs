module Network.Types(LoginStatus(..), LoginName(..), Password(..)) where

import Data.Text(Text)
import Codec.Serialise(Serialise)
import GHC.Generics(Generic)

data LoginStatus =
    LoginSuccess Int -- ^ successful login with the entity ID 
  | LoginFail
  deriving (Show, Generic)

newtype LoginName = LoginName Text
  deriving (Show, Eq, Ord, Generic)

newtype Password = Password Text
  deriving (Show, Eq, Ord, Generic)

instance Serialise LoginName
instance Serialise Password
instance Serialise LoginStatus
