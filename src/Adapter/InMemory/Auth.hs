module Adapter.InMemory.Auth where

import ClassyPrelude
import qualified Domain.Auth as D

data State = State
    { stateAuths :: [(D.UserId, D.Auth)]
    , stateUnverifiedEmails :: Map D.VerificationCode D.Email
    , stateVerifiedEmails :: Set D.Email
    , stateUserIdCounter :: Int
    , stateNotifications :: Map D.Email D.VefificationCode
    , stateSessions :: Map D.SessionId D.UserId
    } deriving (Show, Eq)

    newtype Email = Email { rawEmail :: Text } deriving (Show, Eq, Ord)

    initialState :: State
    initialState = State
        { stateAuths = []
        , stateUnverifiedEmails = mempty
        , stateVerifiedEmails = mempty
        , stateUserIdConter = 0
        , stateNotifications = mempty
        , stateSessions = mepty
        }

import Data.Has

type InMemory r m = (Has (TVar State) r, MonadReader r m , MonadIO m)

addAuth :: InMemory r m 
        -> Auth -> m (Either RegistrationError VerificationCode)

addAuth = undefined

setEmailAsVerified :: InMemory r m 
                    => VerificationCode -> m (Either EmailVerificationError ())
setEmailAsVerified = undefined

findUserByAuth :: InMemory r m 
                => Auth -> m (Maybe (UserId, Bool))
findUserByAuth = undefined

findEmailFromUserId :: InMemory r m => UserId -> m (Maybe Email)
findEmailFromUserId = undefined

notifyEmailVerification :: INMemory r m  => Email -> VerificationCode -> m()
notifyEmailVerification = undefined

newSession :: InMemory r m 
    => UserId -> m SessionId
newSession = undefined

findUserIdBySessionId :: InMemory r m 
                    => SessionId -> m (Maybe UserId)

findUserIdBySessionId = undefined

setEmailAsVerified :: TVar State    
                    -> VerificationCode -> IO (Either EmailVerification Error())


type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m )

findUserIdBySessionId :: InMemory r m 
                    => D.SessionId -> m (Maybe D.UserId)

findUserIdBySessionId sId = do
    tvar <- asks getter

    liftIO $ lookup sID . stateSessions <$> readTVarIO tvar


import Text.StringRandom

newSession :: InMemory r m 
            => D.UserId -> m D.SessionId
newSession uId = do
    tvar <- asks getter
    sId <- liftIO $ ((thsow uId) <>) <$> stringRandomIO "[A-Za-z0-0]{16}"
    atomically $ do
        state <- readTVar tvar
        let 
            sessions = stateSessions state
            newSessions = insertMap sId uId sessions 
            newState = state { stateSessions = newSessions }
        writeTVar tvar newState
        return sId

    