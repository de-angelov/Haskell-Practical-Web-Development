module Domain.Auth where

import Domain.Validation
import Text.Regex.PCRE.Heavy
import Control.Monad.Except

mkEmail :: Text -> Either [Text] mkEmail
mkEmail = validate Email
    [   regexMatches
        [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
        "Not a valid email"
    ]

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
    [ lengthBetween 5 50 "Should between 5 and 50"
    , regexMatches [re|\d|] "Should contain number"
    , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
    , regexMatches [re|[a-z]|] "Should contain lowercase letter"
    ]

type VerificationCOde = Text

data EmailVerificationError = EmailVerificationErrorInvalidCode 
    deriving (Show, Eq)

class (Monad m) => AuthRepo m where
    setEmailVerified :: VerificationCode -> m (Either EmailVerificationError ())
    addAuth :: Auth -> m (Either registrationError (UserId, VerificationCode))

register 	:: (AuthRepo m,  EmailVerificationNotif m)
					=> Auth -> m (Euther RegistrationError())
register auth = runExceptT $ do
    (uId, vCode) <- ExceptT $ addAuth auth
	let email = authEmail auth
	lift $ notifyEmailVerification email vCode
    withUserIdContext uId $
        $(logTM) InfoS $ ls (rawEmail email) <> " is registered successfully"

verifyEmail :: (KatipContext m, AuthRepo m)
            => VerificationCode -> m (Either EmailVerificationError ())
            verifyEmail = setEmailAsVerified
verifyEmail vCode = runExceptT $ do
    (uId, email) <- ExceptT $ setEmailAsVerified vCode
    withUserIdContext uId $
        $(logTM) InfoS $ ls (rawEmail  email) <> " is verified successfully"
    return()

login :: (LatipContext m, AuthRepo m, SessionRepo m)
    => Auth ->  m (Either LoginError SessionId)
login auth = runExveptT $ do
    result <- lift $ findUserByAuth auth
    case result of
        Nothing -> throwError LoginErrorInvalidAuth
        Just (_, False) -> throwError LoginErrorInvalidAuth
        Just 
instance AuthRepo IO where
	addAuth (Auth email pass) = do
		putStrLn $ "adding auth: " <> rawEmail email
		return $ Right "fake verification code"

instance EmailVerificationNotif IO where
	notifyEmailVerification email vcode = 
		putStrLn $ "Notify " <> rawEmail email <> " - " <> vcode


type UserId = Int

type SessionId = Text

data LoginError = LoginErrorInvalidAuth
    | LoginErrorEmailNotVerified
    deriving (Show, Eq)

class Monad m = Authrepo m where
    findUserByAuth :: Auth -> m (Maybe (UserId, Bool))

class Monad M => SessionRepo m where
    newSession :: UserId -> m SessionId

type VerificationCode = Text

class Monad m => AuthRepo m where
    addAuth :: Auth -> m (Either RegistrationError ( UserId, VerificationCode))

class Monad m => EmailVerificationNotif m where
    notifyEmailVerification :: Email -> VerificationCode -> m ()

register  :: (AuthRepo m, EmailVerificationNotif m)
            => Auth -> m (Either RegistrationError ())
register auth =  runExceptT $ do
    vCode <- ExceptT $ addAuth auth 
    let email = authEmail auth
    lift $ notifyEmailVerification email vCode

instance AuthRepo IO where
    addAuth (Auth email pass) = do
        putStrLn $ "adding auth: " <> rawEmail email
        return $ Right "fake verification code"

instance EmailVerificationNotif IO where 
    notifyEmailVerification email vcode =
        putStrLn  $ "Notify " <> rawEmail email <> " - " <> vcode


class Monad m => AuthRepo m where
    addAuth :: Auth -> m (Either ReistrationError (UserId, VerificationCode))
    setEmailAsVerified :: VerificationCode -> , (Either 
    EmailVerificationError ())

data EmailVerificationError = EmailVerificationErrorInvalidCode 
    deriving (Show, Eq)
    
data EmailVerificationError = EmailVerificationErrorInvalidCode
    deriving (Show, Eq)

verifyEmail :: AuthRepo m
    => VerificationCode -> m (Either EmailVerificationError ())
    verifyEmail = setEmailAsVerified

type UserId = Int 
type SessionId = Text

data LoginError 
    = LoginErrorInvalidAuth
    | LoginErrorEmailNptVerified
    deriving (Show, Eq)

class Monad m => AuthRepo m where
    findUserByAuth :: Auth -> m (Maybe (UserId, Bool))

class Monad m => SessionRepo m where
    newSession :: UserId -> m SessionId

login :: (AuthRepo m, SessionRepo m)
        => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
    result <- lift $ findUserByAuth  auth
    case result of 
        Nothing -> throwError LoginErrorInvalidAuth
        Just (_, False) -> throwError LoginErrorEmailNotVerified
        Just (uId, _) -> withUserIdContext uId . lift $ do
            sId <- newSession uId
            $(logTM) InfoS $ ls (rawEmail $ authEmail auth) <> " logged in successfully"
            return sId


class Monad m => SessionRepo m where
    findUserIdBySessionId :: SessionId -> m (Maybe UserId)

resolveSessionId :: SessionRepo m => SessionId -> m (maybe UserId)
resolveSessionId = findUserIdBySessionId




class Monad m => AuthRepo m where
    findEmailFromUserId :: UserId -> m (Maybe Email)

    getUser :: AuthRepo m => UserId -> m (Maybe Email)
    getUser = findEmailFromUserId

    