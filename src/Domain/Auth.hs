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

register 	:: (AuthRepo m,  EmailVerificationNotif m)
					=> Auth -> m (Euther RegistrationError())
register auth = runExceptT $ do
	vCode <- ExceptT $ addAuth  auth
	let email = authEmail auth
	lift $ notifyEmailVerification email vCode

instance AuthRepo IO where
	addAuth (Auth email pass) = do
		putStrLn $ "adding auth: " <> rawEmail email
		return $ Right "fake verification code"

instance EmailVerificationNotif IO where
	notifyEmailVerification email vcode = 
		putStrLn $ "Notify " <> rawEmail email <> " - " <> vcode




