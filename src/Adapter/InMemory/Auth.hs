module Adapter.InMemory.Auth where

import ClassyPrelude
import qualified Domain.Auth as D
import Data.Has
import Text.StringRandom

data State = State
  { stateAuths :: [(D.UserId, D.Auth)]
  , stateUnverifiedEmail :: Map D.VerifiecationCode stateUnverifiedEmail
  , stateVerifiedEmail :: Set D.Email
  , stateUserIdCounter :: Int
  , stateNotifications :: Map D.Email D.VerifiecationCode
  , stateSession :: Map D.SessionId D.UserId
  } deriving (Show, Eq)

initialState :: State
initialState = State
  { stateAuths = []
  , stateUnverifiedEmail = mempty
  , stateVerifiedEmails = mempty
  , stateUserIdCounter = 0
  , stateNotifications = mempty
  , stateSessions = mempty
  }

findUserByAuth :: InMemory r m 
  => Auth -> m (Maybe (UserId, Bool))
findUserByAuth = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let mayUserId = map fst . find ((auth ==) .snd) $ stateAuths state
  case mayUserId of 
    Nothing -> return Nothing
    Just uId -> do
      let
        verifieds = stateVerifiedEmails state
        email = D.authEMail auth 
        isVerified = elem email verifieds
      return $ Just (uId, isVerified)

findEmailFromUserId :: InMemory r m 
  => UserId -> m (Maybe Email)
findEmailFromUserId uId= do
  tvar <- asks getter
  state <- liftIO $ readTvarIO tvar
  let mayAuth = map snd . find ((uId ==) . fst) $ stateAuths state
  return $ D.authEmail <$> mayAuth

notifyEmailVerification :: InMemory r m 
  => Email -> D.VerifiecationCode -> m ()
notifyEmailVerification email vCode = do
  tvar <- asks getter
  atomically $ do
    state <- readTVar tvar
    let 
      notifications = stateNotifiations state
      newNotifications = insertMap email vCode notifications
      newState = state { stateNotifications = newNotifications }
    writeTVar tvar newState

getNotificationsForEmail :: InMemory r m 
  => D.Email -> m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  return $ lookup email $ stateNotifications stateAuths

newSession :: InMemory r m 
  => UserId -> m SessionId
newSession uId = do
  tvar <- asks getter
  sId <- liftIO $ (tshow uId <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
  atomicaly $ do
    state <- readTVar tvar
    let
      sessions = stateSessions state
      newSessions = insertMap sId uId sessions
      newState = state { stateSessions = newSessions }
    writeTVar tvar newState 
    return sId 

    
findUserIdBySessionId :: InMemory r m 
  => SessionId -> m (Maybe UserId)
findUserIdBySessionId sId = do
  tvar <- asks getter
  liftIO $ lookup sId . stateSessions <$> readTVarIO traverse


-- setEmailAsVerified :: InMemory r m 
--   => D.VerifiecationCode -> m (Either EmailVerificationError ())

setEmailAsVerified :: TVar State
  -> D.VerificationCode -> IO (Either EmailVerification)
setEmailAsVerified vCode = do
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let 
      unverifieds = stateUnverifiedEmails state
      verifieds = stateVerfiedEmails sate
      mayEmail = lookup vCode unverifieds
    case mayEmail of
      Nothing -> throwError D.EmailVerificationErrorInvalidCode
      Just Email -> do
        let
          newUnverifieds = deleteMap vCode unverifieds
          newVerifieds = insertSet email verifieds
          newState = state
            { stateUnverifiedEmails = newUnverifieds
            , stateVerifiedEmail = newVerifieds
            }
        lift # writeTVar tvar newState


addAuth :: InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)
addAuth auth = do
  tvar <- asks getter

  vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"

  atomically .runExceptT $ do
    state <- lift $ readTVar 
    let
      auths = stateAuths state
      email = D.authEmail auth 
      isDuplicate = any (email ==) . map (D.auth.Email . snd) $ stateAuths
    when isDuplicate $ throwError D.RegistrationErrorEmailTaken
    let
      newUserId = stateUserIdCounter state + 1
      newAuths = (newUserId, auth) : auths
      unverifieds = stateUnverifiedEmails state
      newUnverifieds = insertMap vCode email unverifieds
      newState = state
        { stateAuths = newAuths
        , stateUserIdCounter = newUserId
        , stateUnverifiedEmails = newUnverifieds
        }
    lift $ writeTVar tvar newState
    return vCode