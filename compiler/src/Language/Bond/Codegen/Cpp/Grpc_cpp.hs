-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cpp.Grpc_cpp (grpc_cpp) where

import Data.Monoid
import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import qualified Language.Bond.Codegen.Cpp.Util as CPP

-- | Codegen template for generating /base_name/_grpc.cpp containing
-- definitions of helper functions and schema metadata static variables.
grpc_cpp :: MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)
grpc_cpp cpp file _imports declarations = ("_grpc.cpp", [lt|
#include "#{file}_reflection.h"
#include "#{file}_grpc.h"

//#include <grpc++/impl/codegen/async_stream.h>
#include <grpc++/impl/codegen/async_unary_call.h>
#include <grpc++/impl/codegen/channel_interface.h>
#include <grpc++/impl/codegen/client_unary_call.h>
#include <grpc++/impl/codegen/method_handler_impl.h>
#include <grpc++/impl/codegen/rpc_service_method.h>
#include <grpc++/impl/codegen/service_type.h>
//#include <grpc++/impl/codegen/sync_stream.h>

#{CPP.openNamespace cpp}
#{doubleLineSep 1 grpc declarations}
#{CPP.closeNamespace cpp}
|])
  where
    grpc s@Service {..} = [lt|
static const char* #{declName}_method_names[] = {
  "/helloworld.Greeter/SayHello",
};
|]
      where

    grpc _ = mempty
