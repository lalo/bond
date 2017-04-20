-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cpp.Grpc_cpp (grpc_cpp) where

import Data.List (elemIndex)
import Data.Monoid
import Prelude
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import qualified Language.Bond.Codegen.Cpp.Util as CPP

-- | Codegen template for generating /base_name/_grpc.cpp containing
-- definitions of helper functions and schema metadata static variables.
grpc_cpp :: MappingContext -> String -> [Import] -> [Declaration] -> (String, L.Text)
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
    idl = MappingContext idlTypeMapping [] [] []

    cppType = getTypeName cpp

    request mt = request' (payload mt)
      where
        payload = maybe "void" cppType
        request' params =  [lt|::bond::comm::message<#{padLeft}#{params}>|]
          where
            paramsText = toLazyText params
            padLeft = if L.head paramsText == ':' then [lt| |] else mempty

    response mt = response' (payload mt)
      where
        payload = maybe "void" cppType
        response' params =  [lt|::bond::comm::message<#{padLeft}#{params}>|]
          where
            paramsText = toLazyText params
            padLeft = if L.head paramsText == ':' then [lt| |] else mempty

    grpc s@Service {..} = [lt|
static const char* #{declName}_method_names[] = {
    #{newlineSep 1 methodStrings serviceMethods}
};

std::unique_ptr< #{declName}::Stub> #{declName}::NewStub(const std::shared_ptr< ::grpc::ChannelInterface>& channel, const ::grpc::StubOptions& options) {
  std::unique_ptr< #{declName}::Stub> stub(new #{declName}::Stub(channel));
  return stub;
}

#{declName}::Stub::Stub(const std::shared_ptr< ::grpc::ChannelInterface>& channel)
    : channel_(channel)
    #{newlineSep 1 methodStringsStub serviceMethods}
  {}

#{newlineSep 1 methodDecl serviceMethods}

#{declName}::Service::Service() {
    #{newlineSep 1 addMethod serviceMethods}
}

#{declName}::Service::~Service() {
}

#{newlineSep 1 methodDeclImpl serviceMethods}

|]
      where
        methodStrings Function{..} = [lt|"/#{getDeclTypeName idl s}/#{methodName}",|]
        methodStrings Event{..} = [lt|"/#{getDeclTypeName idl s}/#{methodName}",|]

        index f = maybe (-1) id (elemIndex f serviceMethods)

        methodStringsStub f@Function{..} = [lt|, rpcmethod_#{methodName}_(#{declName}_method_names[#{index f}], ::grpc::RpcMethod::NORMAL_RPC, channel)|]
        methodStringsStub Event{..} = [lt|"",|]

        methodDecl Function{..} = [lt|::grpc::Status #{declName}::Stub::#{methodName}(::grpc::ClientContext* context, const #{request methodInput}& request, #{response methodResult}* response) {
  return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_#{methodName}_, context, request, response);
}

::grpc::ClientAsyncResponseReader< #{response methodResult}>* #{declName}::Stub::Async#{methodName}Raw(::grpc::ClientContext* context, const #{request methodInput}& request, ::grpc::CompletionQueue* cq) {
  return new ::grpc::ClientAsyncResponseReader< #{response methodResult}>(channel_.get(), cq, rpcmethod_#{methodName}_, context, request);
}|]
        methodDecl Event{..} = [lt|"",|]

        addMethod f@Function{..} = [lt|AddMethod(new ::grpc::RpcServiceMethod(
      #{declName}_method_names[#{index f}],
      ::grpc::RpcMethod::NORMAL_RPC,
      new ::grpc::RpcMethodHandler< #{declName}::Service, #{request methodInput}, #{response methodResult}>(
          std::mem_fn(&#{declName}::Service::#{methodName}), this)));
|]
        addMethod Event{..} = [lt|"",|]

        methodDeclImpl Function{..} = [lt|::grpc::Status #{declName}::Service::#{methodName}(::grpc::ServerContext* context, const #{request methodInput}* request, #{response methodResult}* response) {
  (void) context;
  (void) request;
  (void) response;
  return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}|]
        methodDeclImpl Event{..} = [lt|"",|]

    grpc _ = mempty
