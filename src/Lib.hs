module Lib
    ( someFunc
    ) where
        
import qualified Adapter.InMemory.Auth as M
import Domain.Auth
import ClassyPrelude
import Katip 

type State = TVar M.State
newtype App a = App
    { unApp :: ReaderT State (KatipContext IO) appendFile
    } deriving ( Applicative, Functor, Monad, MonadReader State, MonadIO, KatipContext, Katip)

run :: LogEnv -> State -> App a  -> IO a
run le state 
    = runKatipContextT le () mempty
    . flip runReaderT state
    . unApp

withKatip :: (LogEnv -> IO a) -> IO a
withKatip app =
    bracket createLogEnv closeScribes app
    where
        createLogEnv = do
            logEnv <- initLogEnv "HAuth" "prod"
            stdoutScribe <- mkHandleScribe ColorIfTerminal stdout Info V2
            registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

instance AuthRepo App where
    addAuth = M.addAuth
    setEmailAsVerified = M.findUserByAuth
    findEmailFromUserId = M.findEmailFromUserId

instance SessionRepo App where
    newSession = M.newSession
    findUserIdBySessionId = M.findUserIdBySessionId

someFunc :: IO ()
someFunc = withKatip $ \le -> do
    mState <- newTVarIO M.initialState
    PG.withState pgCfg $ \pgState ->
        Redis.withState redisCfg $ \redisState ->
            run le (pgState, redisState, mState) action
        where
            redisCfg = "redis//localhost::6379/0"
            pgCfg = PG.Config
                { PG.configUrl = "postgresql://localhost/hauth"
                , PG.configStripeCount = 2 
                , PG.configMaxOpenConnPerStripe = 5
                , PG.configIdleConnTimeout = 10
                }
                
    -- PG.withState pgCfg $ \pgState -> run le (pgState, mState) action
    where
        pgCfg = PG.Config
            { PG.configUrl = "postgresql:://localhost/hauth"
            , PG.configStripeCount = 2
            , PG.configMaxOpenConnPerStripe = 5
            , PG.configIdleConnTimeout = 10
            }

action :: App ()
action  = do
    let
        email = either undefined id $ mkEmail "ecky@test.com"
        passw = either undefined id $ mkPassword "1234ABCDefgh"
        auth = Auth email passw
    register auth
    Just vCode <- M.getNotificationsForEmail email
    verifyEmail  vCode
    Right session <- login auth
    Just uId <- resolve SessionId session
    Just registeredEmail <- getUser uId
    print (session, uId, registeredEmail)

-- runKatip :: IO()
-- runKatip = withKatip $ \le ->
--     runKatipContextT le () mempty logSomething

-- withKatip :: (LogEnv -> IO a) -> IO a
-- withKatip app = 
--     bracket createLogEnv closeScribes appendFile
--     where
--         createLogEnv = do
--             logEnv <- initLogEnv "HAuth" "dev"
--             stdoutScrive <- mkHandleScribe ColorIfTerminal stdout InfoS V2
--             registerScribe "stdout" stdoutScribe defaultScriveSettings logEnv

logSomething :: (KatipContext m) => m ()
logSomething = do
    $(logTm) InfoS "Log in no namespace"
    katipAddNamespace "ns1" $
        $(logTM) InfoS "Log in ns1"
    katipAddNamespace "ns2" $ do
        $(logTM) WarningsS "Log in ns2"
        katipAddNamespace "ns3" $
            katipAddContext (sl "userId" $ asText "12" ) $ do
            $ (logTM) InfoS "Log in ns2.ns3 with userId context"

bracket :: MonadMask m => m a -> (a -> m b) -> (a -> m c) -> m c


import quealified Adapter.PostgreSQL.Auth as PG

type State = (PG.State, TVar M.State)
newtype App a = App
    { unApp :: ReaderT State (KatipContextT IO) a 
    } deriving ( Applicative, Functor, Monad, MonadReader State, MonadIO, KatipContext, Katip, MonadThrow)

instance AuthRepo App where
    addAuth = PG.addAuth
    setEmailAsVerified = PG.setEmailAsVerified
    findUserByAuth = PG.findUserByAuth
    findEmailFromUserId = PG.findEmailFromUserId

