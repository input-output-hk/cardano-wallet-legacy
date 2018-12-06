{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Integration.APISanityChecks
    ( spec
    ) where

import           Universum hiding (ByteString)

import           Data.ByteString.Lazy (ByteString)
import           GHC.TypeLits (KnownSymbol)
import           Network.HTTP.Client (Manager, Request, Response, httpLbs,
                     method, responseBody, responseStatus)
import           Network.HTTP.Types (methodPut, status204, status404)
import           Servant ((:>), QueryFlag)
import           Servant.QuickCheck (Args (..), BaseUrl (..), Predicates,
                     RequestPredicate (..), Scheme (..), not500, (<%>))
import           Servant.QuickCheck.Internal (HasGenRequest (..),
                     PredicateFailure (..), serverSatisfiesMgr)
import           Test.Hspec (Spec, it)

import           Cardano.Wallet.API (internalAPI, v1API)
import           Cardano.Wallet.API.Request (FilterBy, SortBy)
import           Cardano.Wallet.API.Types (Tags, WithDefaultApiArg)
import           Cardano.Wallet.API.V1.Parameters (WalletRequestParams,
                     WithWalletRequestParams)
import           Pos.Util.Servant (CustomQueryFlag)


-- Our API apparently is returning JSON Arrays which is considered bad practice as very old
-- browsers can be hacked: https://haacked.com/archive/2009/06/25/json-hijacking.aspx/
-- The general consensus, after discussing this with the team, is that we can be moderately safe.
-- stack test cardano-sl-wallet-new --fast --test-arguments '-m "Servant API Properties"'
spec :: Manager -> Spec
spec mgr = do
    it "v1 API Sanity Checks" $
        serverSatisfiesMgr v1API mgr baseUrl args predicates
    it "internal API Sanity Checks" $
        serverSatisfiesMgr internalAPI mgr baseUrl args predicates
  where
    baseUrl :: BaseUrl
    baseUrl = BaseUrl
        { baseUrlScheme = Https
        , baseUrlHost = "localhost"
        , baseUrlPort = 8090
        , baseUrlPath = ""
        }

    args :: Args
    args = Args
      { replay          = Nothing
      , maxSuccess      = 1000
      , maxDiscardRatio = 10
      , maxSize         = 100
      , chatty          = True
      , maxShrinks      = maxBound
      }

    -- | All the predicates we want to enforce in our API.
    predicates :: Predicates
    predicates = not500
        <%> ifRequest ((methodPut ==) . method) putIdempotency
        <%> RequestPredicate noEmptyBody
        <%> mempty


--
-- Predicates
--

-- | Checks that every PUT request is idempotent. Calling an endpoint with a PUT
-- twice should return the same result.
putIdempotency :: Request -> Manager -> IO [Response ByteString]
putIdempotency req mgr = do
    resp1 <- httpLbs req mgr
    resp2 <- httpLbs req mgr
    let body1 = responseBody resp1
    let body2 = responseBody resp2
    when (body1 /= body2) $
        throwM $ PredicateFailure "putIdempotency" (Just req) resp1
    return [resp1, resp2]

-- | Checks that every request which is not a 204 No Content
-- does not have an empty body, but it always returns something.
noEmptyBody :: Request -> Manager -> IO [Response ByteString]
noEmptyBody req mgr = do
    resp <- httpLbs req mgr
    let body   = responseBody resp
    let status = responseStatus resp
    when (status `notElem` [status204, status404] && body == mempty) $
        throwM $ PredicateFailure "noEmptyBody" (Just req) resp
    return [resp]


--
-- Helpers
--

ifRequest
    :: (Request -> Bool)
    -> (Request -> Manager -> IO [Response ByteString])
    -> RequestPredicate
ifRequest precondition predicate = RequestPredicate $ \req mgr ->
    if precondition req then predicate req mgr else return []


--
-- Instances to allow use of `servant-quickcheck`.
--

instance HasGenRequest (apiType :> sub)
    => HasGenRequest (WithDefaultApiArg apiType :> sub) where
    genRequest _ = genRequest (Proxy @(apiType :> sub))

instance HasGenRequest sub
    => HasGenRequest (SortBy syms res :> sub) where
    genRequest _ = genRequest (Proxy @sub)

instance HasGenRequest sub
    => HasGenRequest (FilterBy syms res :> sub) where
    genRequest _ = genRequest (Proxy @sub)

instance HasGenRequest sub
    => HasGenRequest (Tags tags :> sub) where
    genRequest _ = genRequest (Proxy :: Proxy sub)

instance HasGenRequest (sub :: *)
    => HasGenRequest (WalletRequestParams :> sub) where
    genRequest _ = genRequest (Proxy @(WithWalletRequestParams sub))

instance ( KnownSymbol sym, HasGenRequest sub)
    => HasGenRequest (CustomQueryFlag sym flag :> sub) where
    genRequest _ = genRequest (Proxy @(QueryFlag sym :> sub))
