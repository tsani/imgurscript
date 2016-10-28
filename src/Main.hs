{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import Data.Monoid ( (<>) )
import Data.Proxy
import Data.String ( IsString(..) )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8 )
import Data.Time.Clock ( UTCTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Options.Applicative
import Servant.API hiding ( addHeader )
import Servant.Client
import Servant.Common.Req ( Req, addHeader )
import System.Directory
import System.Exit ( exitFailure )
import System.FilePath

data ImageResponse
  = ImageResponse
    { imageResponseId :: !Text
    , imageResponseTitle :: !(Maybe Text)
    , imageResponseDescription :: !(Maybe Text)
    , imageResponseDatetime :: !UnixEpoch
    , imageResponseIsAnimated :: Bool
    , imageResponseWidth :: !Int
    , imageResponseHeight :: !Int
    , imageResponseSize :: !Int
    , imageResponseViews :: !Int
    , imageResponseBandwidth :: !Int
    , imageResponseDeleteHash :: !(Maybe Text)
    , imageResponseName :: !(Maybe Text)
    , imageResponseSection :: !(Maybe Text)
    , imageResponseLink :: !Text
    , imageResponseGifv :: !(Maybe Text)
    , imageResponseMp4 :: !(Maybe Text)
    , imageResponseMp4Size :: !(Maybe Int)
    , imageResponseIsLooping :: !(Maybe Bool)
    , imageResponseIsFavorite :: !(Maybe Bool)
    , imageResponseIsNsfw :: !(Maybe Bool)
    , imageResponseVote :: !(Maybe Text)
    , imageResponseIsInGallery :: !(Maybe Bool)
    , imageResponseAccountUrl :: !(Maybe Text)
    , imageResponseAccountId :: !(Maybe Int)
    , imageResponseIsAd :: !(Maybe Bool)
    }
  deriving (Eq, Ord, Read, Show)

instance FromJSON ImageResponse where
  parseJSON (Object o) = ImageResponse
    <$> o .: "id"
    <*> o .:? "title"
    <*> o .:? "description"
    <*> o .: "datetime"
    <*> o .: "animated"
    <*> o .: "width"
    <*> o .: "height"
    <*> o .: "size"
    <*> o .: "views"
    <*> o .: "bandwidth"
    <*> o .:? "deletehash"
    <*> o .:? "name"
    <*> o .:? "section"
    <*> o .: "link"
    <*> o .:? "gifv"
    <*> o .:? "mp4"
    <*> o .:? "mp4_size"
    <*> o .:? "looping"
    <*> o .:? "favorite"
    <*> o .:? "nsfw"
    <*> o .:? "vote"
    <*> o .:? "in_gallery"
    <*> o .:? "account_url"
    <*> o .:? "account_id"
    <*> o .:? "is_ad"

-- | UTCTime de/serialized to JSON in seconds-since-epoch form.
newtype UnixEpoch
  = UnixEpoch
    { unUnixEpoch :: UTCTime
    }
  deriving (Eq, Ord, Read, Show)

instance FromJSON UnixEpoch where
  parseJSON (Number n)
    = pure (UnixEpoch $ posixSecondsToUTCTime $ realToFrac n)
  parseJSON _ = fail "cannot convert non-number to UnixEpoch"

data ImageUploadType
  = UploadBinary
  | UploadBase64
  | UploadUrl
  deriving (Eq, Ord, Read, Show)

instance ToJSON ImageUploadType where
  toJSON = \case
    UploadBinary -> "file"
    UploadBase64 -> "base64"
    UploadUrl -> "url"

data ImageUpload imgty
  = ImageUpload
    { imageUploadImage :: imgty
      -- ^ The image content to upload
    , imageUploadAlbum :: !(Maybe Text)
      -- ^ The id of the album you want to add the image to.
      -- For anonymous albums, {album} should be the deletehash that is
      -- returned at creation.
    , imageUploadType :: !(Maybe ImageUploadType)
      -- ^ The type of data uploaded.
    , imageUploadFileName :: !(Maybe Text)
      -- ^ The name of the file being uploaded. This is inferred automatically
      -- if a multipart/form-data post is being made.
    , imageUploadTitle :: !(Maybe Text)
      -- ^ The title of the image.
    , imageUploadDescription :: !(Maybe Text)
    }

instance ToJSON imgty => ToJSON (ImageUpload imgty) where
  toJSON ImageUpload {..} = object
    [ "image" .= imageUploadImage
    , "album" .= imageUploadAlbum
    , "type" .= imageUploadType
    , "name" .= imageUploadFileName
    , "title" .= imageUploadTitle
    , "description" .= imageUploadDescription
    ]

data ImgurJsonResponse d
  = ImgurJsonResponse
    { responseData :: d
    , responseSuccess :: !Bool
    , responseStatus :: !Int
    }
  deriving (Eq, Ord, Read, Show)

instance FromJSON d => FromJSON (ImgurJsonResponse d) where
  parseJSON (Object o) = ImgurJsonResponse
    <$> o .: "data"
    <*> o .: "success"
    <*> o .: "status"
  parseJSON _ = fail "cannot parse non-object to ImgurJsonResponse"

-- | A bytestring containing a base64-encoding of some binary data.
newtype Base64ByteString
  = Base64ByteString
    { unBase64ByteString :: ByteString
    }

instance ToJSON Base64ByteString where
  toJSON (Base64ByteString bs) = toJSON (decodeUtf8 bs)

base64encode :: ByteString -> Base64ByteString
base64encode = Base64ByteString . B64.encode

type ImgurApiV3
  = AuthProtect ImgurAuth
    :> "image"
    :> ReqBody '[JSON] (ImageUpload Base64ByteString)
    :> Post '[JSON] (ImgurJsonResponse ImageResponse)

imgurApiV3 :: Proxy ImgurApiV3
imgurApiV3 = Proxy

image
  :: AuthenticateReq (AuthProtect ImgurAuth)
  -> ImageUpload Base64ByteString
  -> ClientM (ImgurJsonResponse ImageResponse)
image = client imgurApiV3

main :: IO ()
main = execParser opts >>= optMain where
  opts
    = info (helper <*> cliAction) (
      fullDesc <> progDesc "imgur cli" <> header "imgurscript - imgur cli"
    )

  cliAction :: Parser CliAction
  cliAction = subparser (command "upload" (info upload mempty)) where
    upload = UploadImage <$> argument str (metavar "FILE")

data CliAction
  = UploadImage
    FilePath

baseUrl :: IsString a => a
baseUrl = "https://api.imgur.com/3/"

data ImgurAuth

instance ToHttpApiData ByteString where
  toHeader = id
  toUrlPiece = decodeUtf8

type instance AuthClientData (AuthProtect ImgurAuth) = ByteString

authenticateReq :: ByteString -> Req -> Req
authenticateReq s = addHeader "Authorization" clientId where
  clientId :: ByteString
  clientId = "Client-ID " <> s

getClientIdFile :: IO FilePath
getClientIdFile = getXdgDirectory XdgConfig ("imgurscript" </> "clientid")

optMain :: CliAction -> IO ()
optMain = \case
  UploadImage path -> do
    manager <- newManager tlsManagerSettings
    b <- base64encode <$> BS.readFile path
    clientId <- fmap (head . C8.lines) . BS.readFile =<< getClientIdFile
    let uploadData = ImageUpload b Nothing Nothing Nothing Nothing Nothing
    res <- runClientM
      (image (mkAuthenticateReq clientId authenticateReq) uploadData)
      (ClientEnv manager (BaseUrl Https "api.imgur.com" 443 "/3"))
    case res of
      Right ImgurJsonResponse {responseData} -> do
        let ImageResponse {..} = responseData
        putStrLn (T.unpack imageResponseLink)
      Left e -> do
        print e
        exitFailure
