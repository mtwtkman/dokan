{-# LANGUAGE NamedFieldPuns #-}

module Dokan.Listener.Dns (runDns) where

import Control.Exception (bracket)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BS
import Data.IP (toIPv4, toIPv6)
import qualified Data.Map as M
import Dokan.Types (
  DokanConfig (DokanConfig, dokanHostExacts, dokanHostWildcards),
  HostExactIndexId (HostExactIndexId),
  HostPattern (HostWildcard),
  IP (IPv4, IPv6),
  Route (Route, routeDns, routeHostPattern),
 )
import Network.DNS (
  DNSFlags (qOrR),
  DNSHeader (flags),
  DNSMessage (answer, header, question),
  Domain,
  QorR (QR_Response),
  Question (Question, qname),
  RData (RD_A, RD_AAAA),
  ResourceRecord (
    ResourceRecord,
    rdata,
    rrclass,
    rrname,
    rrttl,
    rrtype
  ),
  TYPE (A, AAAA),
  classIN,
  decode,
  encode,
 )
import Network.Socket (
  AddrInfo (
    addrAddress,
    addrFamily,
    addrFlags,
    addrProtocol,
    addrSocketType
  ),
  AddrInfoFlag (AI_PASSIVE),
  Socket,
  SocketOption (ReuseAddr),
  SocketType (Datagram),
  bind,
  close,
  defaultHints,
  getAddrInfo,
  setSocketOption,
  socket,
  withSocketsDo,
 )
import Network.Socket.ByteString (recvFrom, sendTo)

runDns :: DokanConfig -> IO ()
runDns config = do
  let port = "5353"
  putStrLn $ "Starging DNS listener on port " <> port
  withSocketsDo $ bracket (openSocket port) close $ \sock -> forever $ do
    (bs, addr) <- recvFrom sock 512
    case decode bs of
      Left _ -> putStrLn "Failed to decode DNS query"
      Right msg -> do
        response <- handleQuery config msg
        result <- sendTo sock (encode response) addr
        print $ "[DNS] result: " <> show result
        return ()

openSocket :: String -> IO Socket
openSocket port = do
  let hints = defaultHints{addrFlags = [AI_PASSIVE], addrSocketType = Datagram}
  addr : _ <- getAddrInfo (Just hints) Nothing (Just port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  return sock

handleQuery :: DokanConfig -> DNSMessage -> IO DNSMessage
handleQuery config msg = do
  let qs = question msg
  answers <- mapM (resolveQuestion config) qs
  return
    msg
      { header = (header msg){flags = (flags (header msg)){qOrR = QR_Response}}
      , answer = concat answers
      }

resolveQuestion :: DokanConfig -> Question -> IO [ResourceRecord]
resolveQuestion (DokanConfig{dokanHostExacts, dokanHostWildcards}) (Question{qname}) = do
  let hostname = if BS.last qname == '.' then BS.init qname else qname
  let maybeRoute = case M.lookup (HostExactIndexId (BS.unpack hostname)) dokanHostExacts of
        Just r -> Just r
        Nothing -> findWildcardMatch hostname dokanHostWildcards
  case maybeRoute of
    Nothing -> return []
    Just (Route{routeDns}) -> do
      return [makeARecord qname routeDns]

makeARecord :: Domain -> IP -> ResourceRecord
makeARecord name ip =
  ResourceRecord
    { rrname = name
    , rrtype = case ip of
        IPv4{} -> A
        IPv6{} -> AAAA
    , rrclass = classIN
    , rrttl = 300
    , rdata = case ip of
        IPv4 a b c d -> RD_A (toIPv4 $ fmap fromIntegral [a, b, c, d])
        IPv6 a b c d e f g h -> RD_AAAA (toIPv6 $ fmap fromIntegral [a, b, c, d, e, f, g, h])
    }

findWildcardMatch :: BS.ByteString -> [Route] -> Maybe Route
findWildcardMatch _ [] = Nothing
findWildcardMatch host (route@Route{routeHostPattern} : rest)
  | isMatch host routeHostPattern = Just route
  | otherwise = findWildcardMatch host rest
 where
  isMatch :: BS.ByteString -> HostPattern -> Bool
  isMatch h (HostWildcard (_, p)) =
    let suffix = BS.drop 1 (BS.pack p)
     in suffix `BS.isSuffixOf` h && BS.length h > BS.length suffix
  isMatch _ _ = False
