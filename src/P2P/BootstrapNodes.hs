{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: P2P.BootstrapNodes
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module P2P.BootstrapNodes
( mainnetBootstrapHosts
, testnet04BootstrapHosts
) where

-- internal modules

import Chainweb.HostAddress

-- -------------------------------------------------------------------------- --
-- | Mainnet bootstrap nodes.
--
-- Nodes in this list need a public DNS name and a corresponding TLS
-- certificate. Operators of the nodes are expected to guarantee long term
-- availability of the nodes.
--
-- Please make a pull request, if you like to see your node being included here.
--
mainnetBootstrapHosts :: [HostAddress]
mainnetBootstrapHosts = map unsafeHostAddressFromText
    [ "fr-1.chainweb-community.org:443"
    , "nl-1.chainweb-community.org:443"
    , "pl-1.chainweb-community.org:443"
    , "pl-2.chainweb-community.org:443"
    , "ca-1.chainweb-community.org:443"
    , "us-e1.bootstrap.kadinic.com:443"
    ]

-- -------------------------------------------------------------------------- --
-- | Testnet04 bootstrap nodes.
--
-- Nodes in this list need a public DNS name and a corresponding TLS
-- certificate. Operators of the nodes are expected to guarantee long term
-- availability of the nodes.
--
-- Please make a pull request, if you like to see your node being included here.
--
testnet04BootstrapHosts :: [HostAddress]
testnet04BootstrapHosts = map unsafeHostAddressFromText
    [ "us1.testnet.chainweb.com:443"
    , "us2.testnet.chainweb.com:443"
    , "eu1.testnet.chainweb.com:443"
    , "eu2.testnet.chainweb.com:443"
    , "ap1.testnet.chainweb.com:443"
    , "ap2.testnet.chainweb.com:443"
    ]
