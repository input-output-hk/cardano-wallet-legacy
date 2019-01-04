{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.JsendCompliance where

import           Prelude

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Proxy
import qualified Data.Text as Text
import           Data.Typeable
import           Servant hiding (AppendList)
import           Test.Hspec

import           Servant.Swagger.Internal.Test (props)
import           Servant.Swagger.Internal.TypeLevel (AddBodyType, AppendList,
                     Every, Nub, TMap)
import           Test.QuickCheck (Arbitrary (..), Property, counterexample,
                     (.&&.), (===))

-- | For each endpoint, create a property that verifies that serializing
-- the response for that endpoint is JSend compliant.
checkJsendCompliance
    :: forall proxy api contentType responseTypes.
    ( TMap (Every [Typeable, Show, Arbitrary, ToJSON]) responseTypes
    , responseTypes ~ ResponseTypes contentType api
    )
    => proxy contentType
    -> proxy api
    -> Spec
checkJsendCompliance _ _ =
    props
        (Proxy @'[ToJSON])
        verify
        (Proxy @responseTypes)
  where
    verify :: forall a. ToJSON a => a -> Property
    verify a = case toJSON a of
        Object xs ->
            case HM.lookup "status" xs of
                Just (String status) ->
                    case status of
                        "error" ->
                            checkError xs
                        "fail" ->
                            checkDataAppropriate xs
                        "success" ->
                            checkDataAppropriate xs
                        _ ->
                            failProp $ "Expected one of \"error\", \"fail\", or \"success\" for key \"status\". Instead, saw: " ++ Text.unpack status
                Just other ->
                    failProp $ "Expected \"status\" field to have type String, got: " ++ show other
                Nothing ->
                    failProp $ "Expected \"status\" field in object, not found: " ++ show xs
        wrong ->
            failProp ("Expected Object, received: " ++ show wrong)

    checkError xs =
        case HM.lookup "message" xs of
            Just (String _) ->
                case HM.lookup "code" xs of
                    Just code ->
                        case code of
                            Number _ ->
                                pass
                            _ ->
                                failProp $ "Expected type Number for field \"code\", got: " ++ show code
                    Nothing ->
                        pass
            _ ->
                failProp ("Expected \"message\" field of type String in object with status \"error\", not found: " ++ show xs)

    checkDataAppropriate xs =
        case HM.lookup "data" xs of
            Just _ ->
                pass
            other ->
                failProp $ "Expected \"data\" key in success/fail response, got: " ++ show other

failProp :: String -> Property
failProp msg = counterexample msg False

pass :: Property
pass = True === True

type ResponseTypes c api = Nub (ResponseTypes' c api)

type family ResponseTypes' c api :: [*] where
  ResponseTypes' c (Verb verb 204 cs a) = '[]
  ResponseTypes' c (Verb verb b cs (Headers hdrs a)) = AddBodyType c cs a '[]
  ResponseTypes' c (Verb verb b cs a) = AddBodyType c cs a '[]
  ResponseTypes' c (e :> api) = ResponseTypes' c api
  ResponseTypes' c (a :<|> b) = AppendList (ResponseTypes' c a) (ResponseTypes' c b)
  ResponseTypes' c api = '[]

