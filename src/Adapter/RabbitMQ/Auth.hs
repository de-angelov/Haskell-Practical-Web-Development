data ConnectionOpts = ConnectionOpts {
    coServers :: ![(String, PortNumber)],
    -- ^ A list of host-port pairs.
    coVHost :: !Text,
    -- ^ The VHost to connect to.
    coAuth :: ![SASLMechanism],
    -- ^ The 'SASLMechanism's to use for authenticating with the broker.
    coMaxFrameSize :: !(Maybe Word32),
    -- ^ The maximum frame size to e used. If not specified, no limit is assume.
    coHearthbeatDelay :: !(Maybe Word16)
    -- ^ The delay in seconds for receiving Heartbeat
    coMaxChannel :: !(Maybe Word16),
    -- ^ The maximum number of channels the client will use.
    coTLSSetings :: Maybe TLSSettings,
    -- ^ Whether or not to cennect to servers using TLS.
    oName :: !(Maybe Text)
    -- ^ optional connection name (will be displayed in the RabbitMQ web interface)
}

defaultConnectionOpts { coName = Just "hauth" }

openChannel :: Connection -> IO Channel
closeChannel :: Channel -> IO ()

qos :: Channel -> Word32 -> Word16 -> Bool -> IO ()
qos chan prefetchSize prefetchCount global

addChannelExceptionHandler :: Channel -> (SomeException -> IO ()) -> IO ()
addChannelExceptionHandler chan callback

declareExchange :: Channel -> ExchangeOpts -> IO ()
data ExchangeOpts = ExchangeOpts 
    { exchangeName :: Text
    -- ^ (must be set); the name of the exchange
    , exchangeType :: Text
    -- ^ (must be set); the type of the exchange
    , exchangePassive :: Bool
    -- ^ (default 'False'); If set, the server will not create the exchange.
    , exchangeDurable :: Bool
    -- ^ (default 'True'); Non-durable exchanges are purged if a server restarts.
    , ecxchangeAutoDelete :: Bool
    -- ^ (default 'False');
    -- If set, the exchange is deleted when all queues have finished using it.
    , exchangeArguments :: FieldTable
    -- ^ (default empty); A set of arguments for the declaration.
    }

newExchange
    { exchangeName = "auth"
    , exchangeType = "topic"
    }

declareQueue :: Channel -> QueueOpts -> IO (Text, Int, Int)


data QueueOpts = QueueOpts
    { queueName :: Text,
    -- ^ (default \"\"); the name of the queue;
    -- if left empty, the server will generate a new name
    , queuePassive :: Bool,
    -- ^ (default 'False'); If set, the server will not create the queue.
    , queueDurable :: Bool,
    -- ^ (default 'False');
    -- Exclusive queues may only be consumed from by the current connection.
    , queueHeaders :: FieldTable
    -- ^ (default empty);
    -- Headers to use when creating this queue.
    }

newQueue
    { queueName = "emailVerification"
    , queueDurable = False
    }

bindQueue :: Channel -> Text -> Text -> Text -> IO ()

publishMsg :: Channel -> Text -> Text -> Message -> IO (Maybe Int)
publishMsg channel exchange routingKey msg

confirmSelect :: Channel -> Bool -> IO ()
confirmSelect changel exchange routingKey  msg

confirmSelect :: Channel -> Bool -> IO ()
confirmSelect channel noblock

data Message = Message 
    { msgBody :: BL.ByteString
    -- the context of your message
    , msgDeliveryMode :: Maybe DeliveryMode
    , msgTimestamp :: Maybe  Timestamp
    -- ^ use in any way you like; this doesn't affect the way the message
    , msgID :: Maybe Text
    -- ^ use in any way you like; this doesn’t affect the way the message
    , msgType :: Maybe Text
    -- ^ use in any way you like; this doesn't affect the way the message
    , msgUserID :: Maybe Text
    , msgApplicationID :: Maybe Text
    , msgClusterID :: Maybe Text
    , msgContentType :: Maybe Text
    , msgContextEncoding :: Maybe Tex
    , msgReplyTo :: Maybe Text
    , msgPriority :: Maybe Octet
    , msgCorrelationID :: Maybe Text
    , msgExpiration :: Maybe Text
    , msgHeaders :: Maybe FieldTable
    }


getMsg :: Channel -> Ack -> Text -> IO (Maybe (Message, Envelope))

consumeMsgs :: Channel -> Text -> Ack
            -> ((Message, Envelope) -> IO ())
            -> IO ConsumerTag
consumeMsgs chan queue ack callback

ackEnv :: Envelope -> IO ()
rejectEnv :: Envelope -> Bool -> IO ()
