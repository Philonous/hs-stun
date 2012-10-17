{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Endian where

import Data.Word


-- from network/HsNet.h
#if defined(HAVE_WINSOCK2_H) && !defined(cygwin32_HOST_OS)
# define WITH_WINSOCK 1
#endif

#if !defined(CALLCONV)
# if defined(WITH_WINSOCK)
# define CALLCONV stdcall
# else
# define CALLCONV ccall
# endif
#endif

-- from network/Socket.hs
foreign import CALLCONV unsafe "ntohs" ntohs :: Word16 -> Word16
foreign import CALLCONV unsafe "htons" htons :: Word16 -> Word16
foreign import CALLCONV unsafe "ntohl" ntohl :: Word32 -> Word32
foreign import CALLCONV unsafe "htonl" htonl :: Word32 -> Word32