{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Concurrent (forkIO, killThread, threadDelay)
import           Control.Concurrent.Async (forConcurrently)
import           Control.Exception (throwIO)
import qualified Data.ByteString.Char8 as BS
import           Data.Functor.Contravariant (contramap)
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Network.Transport (EndPointAddress (..), closeTransport)
import qualified Network.Transport.TCP as TCP
import           Prelude (getChar)
import           System.Random
import           Universum

import           ByronSpecifics (MsgBlock (..), MsgGetBlocks (..),
                     MsgGetHeaders (..), MsgHeaders (..), securityParameter,
                     usedVerInfo)

import           Node
import           Pos.Chain.Block (Block, HeaderHash, getBlockHeader, headerHash,
                     prevBlockL)
import           Pos.Core.Chrono (NewestFirst (..))
import           Pos.Infra.Communication.BiP (BiP, bipPacking)
import           Pos.Infra.Communication.Types.Protocol (PeerData)
import           Pos.Util.Trace (stdoutTrace)

type Packing = BiP

data HeaderData = HeaderData {
      headerHashToBeProcessed :: Maybe HeaderHash
    , headerHashesConsumed    :: [HeaderHash]
    } deriving (Show, Eq)

-- the wallet is not going to be redirecting information, just being the consumer in a converse with the node
noListener
    :: NodeId
    -> PeerData
    -> [Listener Packing PeerData]
noListener _ _ = []


queryWorker
    :: NodeId
    -> [NodeId]
    -> IORef HeaderData
    -> Converse Packing PeerData
    -> IO ()
queryWorker anId peerIds headerData = syncLoop
  where
    syncLoop
        :: Converse Packing PeerData
        -> IO ()
    syncLoop converse = loop
        where
        loop :: IO ()
        loop = do
            threadDelay 3000000
            let getCurrentHeader
                    :: NodeId
                    -> ConversationActions MsgGetHeaders MsgHeaders
                    -> IO ()
                getCurrentHeader peerId cactions = do
                    send cactions (MsgGetHeaders [] Nothing)
                    received <- recv cactions maxBound
                    case received of
                        Just (MsgHeaders (NewestFirst (tip:|[]))) -> do
                            let newHeaderHash = headerHash tip
                            (HeaderData _ headerConsumed) <- readIORef headerData
                            writeIORef headerData $ HeaderData (Just newHeaderHash) headerConsumed
                        Just text -> putTextLn $ show anId <> " no headers from " <> show peerId <> " data:" <> show text
                        Nothing -> error "getCurrentHeader Unexpected end of input"
                getBlocks
                    :: NodeId
                    -> ConversationActions MsgGetBlocks MsgBlock
                    -> IO ()
                getBlocks peerId cactions = do
                    (HeaderData headerM pulledBlockHeaders) <- readIORef headerData
                    case headerM of
                        Nothing -> return ()
                        Just headerTip -> do
                            case headerTip `L.elem` pulledBlockHeaders of
                                True -> return ()
                                False -> do
                                    blocks <- getBlock peerId cactions headerTip pulledBlockHeaders []
                                    let headersToAdd = map (headerHash . getBlockHeader) blocks
                                    writeIORef headerData $ HeaderData (Just headerTip) (L.take securityParameter $ headersToAdd ++ pulledBlockHeaders)
                getBlock
                    :: NodeId
                    -> ConversationActions MsgGetBlocks MsgBlock
                    -> HeaderHash
                    -> [HeaderHash]
                    -> [Block]
                    -> IO [Block]
                getBlock peerId cactions currentHeader consumedHeaders blocks = do
                    send cactions (MsgGetBlocks currentHeader currentHeader)
                    received <- recv cactions maxBound
                    case received of
                        Just (MsgBlock block@(Right _)) -> do
                            -- main block
                            putTextLn "downloaded main block from trusted node :"
                            putTextLn $ show block
                            let previousHeader = block ^. prevBlockL
                            case ((previousHeader `L.elem` consumedHeaders) || (L.length consumedHeaders ==0)) of
                                True ->
                                    pure $ block : blocks
                                False ->
                                    getBlock peerId cactions previousHeader consumedHeaders (block : blocks)
                        Just (MsgBlock block@(Left _)) -> do
                            -- genesis block
                            putTextLn "downloaded genesis block from trusted node"
                            let previousHeader = block ^. prevBlockL
                            case ((previousHeader `L.elem` consumedHeaders) || (L.length consumedHeaders ==0)) of
                                True ->
                                    pure $ blocks
                                False ->
                                    getBlock peerId cactions previousHeader consumedHeaders blocks
                        Just (MsgNoBlock txt) -> do
                            putTextLn $ show txt
                            pure blocks
                        _ ->
                            pure blocks
            _ <- forConcurrently peerIds $ \peerId -> do
                converseWith converse peerId (\_ -> Conversation (getCurrentHeader peerId))
                converseWith converse peerId (\_ -> Conversation (getBlocks peerId))
            loop


main :: IO ()
main = do

    let params = TCP.defaultTCPParameters { TCP.tcpCheckPeerHost = True }
    transport <- do
        transportOrError <-
            TCP.createTransport (TCP.defaultTCPAddr "127.0.0.1" "3005") params
        either throwIO return transportOrError

    let prng1 = mkStdGen 0

    let convEstablishTimeout = 30000000
    let nodeEnv = defaultNodeEnvironment { nodeAckTimeout = convEstablishTimeout }

    -- this is the trusted node we are going to talk with
    let peerNodeId = NodeId $ EndPointAddress $ BS.pack "127.0.0.1:3001:0"

    -- later it would be read from persisted storage
    initialHeaderData <- newIORef $ HeaderData Nothing []

    putTextLn "Starting decoupled block listener"
    node (contramap snd stdoutTrace) (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay)
         prng1 bipPacking usedVerInfo nodeEnv $ \theNode ->
        NodeAction (noListener . nodeId $ theNode) $ \converse -> do
                    tid1 <- forkIO $ queryWorker (nodeId theNode) [peerNodeId] initialHeaderData converse
                    putTextLn "Press return to stop"
                    _ <- getChar
                    killThread tid1
                    putTextLn "Stopping requestTipWorker"


    --we will need to persist that
    finalHeaderHashes <- readIORef initialHeaderData
    putTextLn $ show finalHeaderHashes

    putTextLn "Decoupled block listener stopped."
    closeTransport transport
