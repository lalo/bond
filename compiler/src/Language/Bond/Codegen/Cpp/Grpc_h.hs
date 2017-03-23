-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cpp.Grpc_h (grpc_h) where

import System.FilePath
import Data.Monoid
import Prelude
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder
import Text.Shakespeare.Text
import Language.Bond.Util
import Language.Bond.Syntax.Types
import Language.Bond.Syntax.Util
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.TypeMapping
import qualified Language.Bond.Codegen.Cpp.Util as CPP


-- | Codegen template for generating /base_name/_grpc.h containing declarations of
-- of service interface and proxy.
grpc_h :: Maybe String -> MappingContext -> String -> [Import] -> [Declaration] -> (String, L.Text)
grpc_h export_attribute cpp file imports declarations = ("_grpc.h", [lt|
#pragma once

#include "#{file}_types.h"
#{newlineSep 0 includeImport imports}

#include <grpc++/impl/codegen/async_stream.h>
#include <grpc++/impl/codegen/async_unary_call.h>
#include <grpc++/impl/codegen/method_handler_impl.h>
#include <grpc++/impl/codegen/proto_utils.h>
#include <grpc++/impl/codegen/rpc_method.h>
#include <grpc++/impl/codegen/service_type.h>
#include <grpc++/impl/codegen/status.h>
#include <grpc++/impl/codegen/stub_options.h>
#include <grpc++/impl/codegen/sync_stream.h>

#{CPP.openNamespace cpp}
    #{doubleLineSep 1 grpc declarations}

#{CPP.closeNamespace cpp}
|])
  where
    includeImport (Import path) = [lt|#include "#{dropExtension path}_grpc.h"|]

    cppType = getTypeName cpp

    request mt = request' (payload mt)
      where
        payload = maybe "void" cppType
        request' params =  [lt|#{params}|]

    response mt = response' (payload mt)
      where
        payload = maybe "void" cppType
        response' params =  [lt|#{params}|]

    grpc s@Service {..} = [lt|class #{declName} final {
 public:
  class StubInterface {
   public:
    virtual ~StubInterface() {}
    // Sends a greeting
    virtual ::grpc::Status SayHello(::grpc::ClientContext* context, const #{request}& request, #{response}* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< #{response}>> AsyncSayHello(::grpc::ClientContext* context, const #{request}& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< #{response}>>(AsyncSayHelloRaw(context, request, cq));
    }
  private:
    virtual ::grpc::ClientAsyncResponseReaderInterface< #{response}>* AsyncSayHelloRaw(::grpc::ClientContext* context, const #{request}& request, ::grpc::CompletionQueue* cq) = 0;
  };
  class Stub final : public StubInterface {
   public:
    Stub(const std::shared_ptr< ::grpc::ChannelInterface>& channel);
    ::grpc::Status SayHello(::grpc::ClientContext* context, const #{request}& request, #{response}* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< #{response}>> AsyncSayHello(::grpc::ClientContext* context, const #{request}& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< #{response}>>(AsyncSayHelloRaw(context, request, cq));
    }

   private:
    std::shared_ptr< ::grpc::ChannelInterface> channel_;
    ::grpc::ClientAsyncResponseReader< #{response}>* AsyncSayHelloRaw(::grpc::ClientContext* context, const #{request}& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_SayHello_;
  };
  static std::unique_ptr<Stub> NewStub(const std::shared_ptr< ::grpc::ChannelInterface>& channel, const ::grpc::StubOptions& options = ::grpc::StubOptions());

  class Service : public ::grpc::Service {
   public:
    Service();
    virtual ~Service();
    // Sends a greeting
    virtual ::grpc::Status SayHello(::grpc::ServerContext* context, const #{request}* request, #{response}* response);
  };
  template <class BaseClass>
  class WithAsyncMethod_SayHello : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_SayHello() {
      ::grpc::Service::MarkMethodAsync(0);
    }
    ~WithAsyncMethod_SayHello() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status SayHello(::grpc::ServerContext* context, const #{request}* request, #{response}* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void RequestSayHello(::grpc::ServerContext* context, #{request}* request, ::grpc::ServerAsyncResponseWriter< #{response}>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(0, context, request, response, new_call_cq, notification_cq, tag);
    }
  };
  typedef WithAsyncMethod_SayHello<Service > AsyncService;
  template <class BaseClass>
  class WithGenericMethod_SayHello : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithGenericMethod_SayHello() {
      ::grpc::Service::MarkMethodGeneric(0);
    }
    ~WithGenericMethod_SayHello() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status SayHello(::grpc::ServerContext* context, const #{request}* request, #{response}* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
  };
  template <class BaseClass>
  class WithStreamedUnaryMethod_SayHello : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithStreamedUnaryMethod_SayHello() {
      ::grpc::Service::MarkMethodStreamed(0,
        new ::grpc::StreamedUnaryHandler< #{request}, #{response}>(std::bind(&WithStreamedUnaryMethod_SayHello<BaseClass>::StreamedSayHello, this, std::placeholders::_1, std::placeholders::_2)));
    }
    ~WithStreamedUnaryMethod_SayHello() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable regular version of this method
    ::grpc::Status SayHello(::grpc::ServerContext* context, const #{request}* request, #{response}* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    // replace default version of method with streamed unary
    virtual ::grpc::Status StreamedSayHello(::grpc::ServerContext* context, ::grpc::ServerUnaryStreamer< #{request},#{response}>* server_unary_streamer) = 0;
  };
  typedef WithStreamedUnaryMethod_SayHello<Service > StreamedUnaryService;
  typedef Service SplitStreamedService;
  typedef WithStreamedUnaryMethod_SayHello<Service > StreamedService;
};|]

    grpc _ = mempty
