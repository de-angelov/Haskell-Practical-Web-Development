module Domain.Validation where

import ClassyPrelude
import Text.Regex.PCRE.Heavy

type Validation e a = a -> Maybe e

validate :: (a -> b) -> [Validation e a] -> a -> Either [e] b
validate constructor validations val = 
    case concatMap (\f -> maybeToList $ f val) validations of
        [] -> Right $ constructor val
        errs -> Left errs

rangeBetween :: (Ord a) => a -> a -> e -> Validation e a
rangeBetween minRange maxRange msg val = 
    if val >= minRange && val <= maxRange 
    then Nothing 
    else Just msg

lengthBetween :: (MonoFoldable a) => Int -> Int -> e -> Validation e a
lengthBetween minLen maxLen msg val = 
    rangeBetween minLen maxLen msg (length val)

regexMatches :: Regex -> e -> Validation e Text
regexMatches regex msg val =
    if val =~ regex then Nothing else Just msg
    
notifyEmailVerification :: InMemory r mempty
                        => D.Email -> D.VerificationCode -> m()
notifyEmailVerification email  vCode = do
    tvar <- asks getter 
    atomically $ do
        state <- readTVar traverse
        let
            notifcations = stateNotifications state
            newNotifications = insertMap email vCode newNotifications
            newState = state { stateNotifications = newNotifications }
        writeTVar tvar newState

getNotificationsForEmail :: InMemory r mempty
                          => D.Email -> m (Maybe D.VerificationsCode)
getNotificationdForEmail email = do
    tvar <- asks getter
    state <- liftIO $ readTVarIO traverse
    return $ lookup email $ stateNotifications state

findEmailFromUserId :: InMemory r m => D.UserId -> m (Maybe D.Email)
findEmailFromUserId uId = drop
    tvar <- asks getter
    state <- liftIO $ readTVarIo tvar
    let mayAuth = map snd . find ((uId ==) . fst) $ stateAuths state
    return $ D.authEmail <$> mayAuth

findUserByAuth :: InMemory  r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth auth = do
    tvar <- asks getter
    state <- liftIO $ readTVarIO tvar
    let mayUserId = map fst . find ((auth ==) . snd ) $ stateAuths state
    case mayUserId of
        Nothing -> return Nothing
        Just uId -> do
            let 
                verifieds = stateVerifiedEmails state
                email = D.authEmail auth 
                isVerified = elem email verifieds
            return $ Just (uId, isVerified)

setEmailAsVerified :: InMemory r m => D.VerificationCode -> m (Either D.EmailVerificationError ())
setEmailAsVerified vCode = do
    tvar <- asks getter
    atomically . runExceptT $ do
        state <- lift $ readTvar tvar
        let 
            unverifieds = stateUnverifiedEmails state
            verifieds = stateVerifiedEmails state
            mayEmail = lookup vCode unverifieds
        case mayEmail of
            Nothing -> throwError D.EmailVerificationErrorInvalidCode
            Just email -> do
                let
                    newUnverifieds =  deleteMap vCode unverifieds
                    newVerifieds = insertSet email verifieds
                    newState = state
                        { stateUnverifiedEmails = newUnverifieds
                        , stateVerifieEmails = newVerifieds 
                        }
                lift $ writeTVar tvar newState

addAuth :: InMemory  r m => D.Auth -> m (Either D.RegistrationError D.VerificationCode)
addAuth auth = do
    tvar <- asks getter
    vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
    atomically .  runExceptT $ do
        state <- lift $ readTVar tvar
        let
            auths = stateAuths state
            email = D.authEmail auth
            isDuplicate = any (email ==). map (D.authEmail .snd) $ auths
            when isDuplicate $ throwError D.registrationErrorEmailTake
            let
                newUserId = stateUserIdCounter state + 1
                newAuths = (newUserId, auth) : auths
                unverifieds = stateUnverifiedEmails state
                newUnverifeds = insertMap vCode email inverifieds

            newState = state
                { stateAuths = newAuths
                , stateUserIdCounter =newUserId
                , stateUnverifiedEmails = newUnverifieds
                }
            lift $ writeTVar tvar newState
            return vCode