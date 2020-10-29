module Domain.Auth (
  Auth(..),
  Email,
  mkEmail,
  Password,
  mkPassword,
  UserId,
  VerificationCode,
  SessionId,
  RegistrationError(..),
  EmailVerificationError(..),
  LoginError(..),
  AuthRepo(..),
  EmailVerificationNotif(..),
  SessionRepo(..)
) where

import ClassyPrelude
import Domain.Validation
import Text.Regex.PCRE.Heavy
import Control.Monad.Except

newtype Email = Email { emailRaw :: Text }
  deriving (Show, Eq, Ord)

rawEmail :: Email -> Text
rawEmail = emailRaw

data EmailValidationErr = EmailValidationErrInvalidEmail

mkEmail :: Text -> Either [Text] Email
mkEmail = validate Email
  [ regexMatches
    [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
    "Not a valid email"
  ]

data PasswordValidationErr = PasswordValidationErrLength Int
  | PasswordValidationErrMustContainUpperCase
  | PasswordValidationErrMustContainLowerCase
  | PasswordValidationErrMustContainNumber

newtype Password = Password { passwordRaw :: Text } 
  deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
  [ lengthBetween 5 50 "Should between 5 and 50"
  , regexMatches [re|\d|] "Should contain number"
  , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
  , regexMatches [re|[a-z]|] "Should contain lowercase letter"
  ]

data Auth = Auth
  { authEmail :: Email
  , authPassword :: Password
  } deriving (Show, Eq)

type UserId = Int

type VerificationCode = Text

class Monad m  => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())
  findUserByAuth ::  Auth -> Maybe (UserId, Bool)
  findEmailFromUserId :: UserId -> m (Maybe Email)
-- class Monad m => AuthRepo m where
--   addAuth :: Auth -> m (Either RegistrationError VerificationCode)

class Monad m => EmailVerificationNotif where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

register :: (AuthRepo m, EmailVerificationNotif m)
  => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do 
  vCode <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode

instance AuthRepo IO where
  addAuth (Auth email pass) = do
    putStrLn $ "adding auth: " <> rawEmail email
    return $ Right "fake verification code"

instance EmailVerificationNotif IO where
  notifyEmailVerification email vcode =
    putStrnLn $ "Notify : " <> rawEmail email <> " - " <> vcode

type SessionId = Text

data RegistrationError 
  = RegistrationErrorEmailTaken
  deriving (Show, Eq)

data EmailVerificationError
  = EmailVerificationErrorInvalidCode
  deriving (Show, Eq)

verifyEmail :: AuthRepo m 
  => VerificationCode -> m (Either EMailVerificationError ())
verifyEmail = setEmailAsVerified

data LoginError
  = LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified
  deriving (Show, Eq)

class Monad m => AuthRepo m where
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)

login :: (AuthRepo m, SessionRepo m)
  => Auth -> m (Either LoginError SessionId)
login = undefined

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
reolveSessionId = findUserBySessionId

login :: (AuthRepo m, SessionRepo m)
  => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (uId, _) -> lift $ newSession uId

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId



-- login :: (AuthRepo m, SessionRepo m)
--   => Auth -> m (Either LoginError SessionId)
-- login auth = runExceptT $ do 
--   result <- lift $ findUserByAuth auth
--   case result of
--     Nothing -> throwError LoginErrorInvalidAuth
--     Just (_, False) -> throwError LoginErrorEmailNotVerified
--     Just (uId, _) -> lift $ newSession uId

-- resolveSessionId :: (SessionRepo m) => SessionId -> m (Maybe UserId)
-- resolveSessionId = findUserIdBySessionId

-- getUser :: (AuthRepo m) => UserId -> m (Maybe Email)
-- getUser = findEmailFromUserId