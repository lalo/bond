
#pragma once

#include "generic_service_reflection.h"
#include "generic_service_types.h"

// todo: remove message
#include <bond/comm/message.h>
#include <bond/ext/grpc/bond_utils.h>
#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/unary_call.h>
#include <bond/ext/grpc/detail/client_call_data.h>
#include <bond/ext/grpc/detail/service.h>
#include <bond/ext/grpc/detail/service_call_data.h>

#include <boost/optional/optional.hpp>
#include <functional>
#include <memory>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4100 4267)
#endif

#include <grpc++/impl/codegen/channel_interface.h>
#include <grpc++/impl/codegen/client_context.h>
#include <grpc++/impl/codegen/completion_queue.h>
#include <grpc++/impl/codegen/rpc_method.h>
#include <grpc++/impl/codegen/status.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace tests
{

class Foo final
{
public:
    template <typename TThreadPool>
    class Client
    {
    public:
        Client(const std::shared_ptr< ::grpc::ChannelInterface>& channel, std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager, TThreadPool* threadPool);

        void Asyncfoo31(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, std::function<void(const ::bond::comm::message<void>&, const ::grpc::Status&)> cb);

        void Asyncfoo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message<Payload>&, const ::grpc::Status&)> cb);

        void Asyncfoo33(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, std::function<void(const ::bond::comm::message<Payload>&, const ::grpc::Status&)> cb);

        Client(const Client&) = delete;
        Client& operator=(const Client&) = delete;

        Client(Client&&) = default;
        Client& operator=(Client&&) = default;

    private:
        std::shared_ptr< ::grpc::ChannelInterface> channel_;
        std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager_;
        TThreadPool* threadPool_;

        const ::grpc::RpcMethod rpcmethod_foo31_;

        const ::grpc::RpcMethod rpcmethod_foo32_;

        const ::grpc::RpcMethod rpcmethod_foo33_;
    };

    class Service : public ::bond::ext::gRPC::detail::service
    {
    public:
        Service()
        {
            AddMethod("/tests.Foo/foo31");
            AddMethod("/tests.Foo/foo32");
            AddMethod("/tests.Foo/foo33");
        }

        virtual ~Service() { }
        virtual void start(::grpc::ServerCompletionQueue* cq) override
        {
            BOOST_ASSERT(cq);

            _rd_foo31.emplace(this, 0, cq, std::bind(&Service::foo31, this, std::placeholders::_1));
            _rd_foo32.emplace(this, 1, cq, std::bind(&Service::foo32, this, std::placeholders::_1));
            _rd_foo33.emplace(this, 2, cq, std::bind(&Service::foo33, this, std::placeholders::_1));

            queue_receive(0, &_rd_foo31->_receivedCall->_context, &_rd_foo31->_receivedCall->_request, &_rd_foo31->_receivedCall->_responder, cq, &_rd_foo31.get());
            queue_receive(1, &_rd_foo32->_receivedCall->_context, &_rd_foo32->_receivedCall->_request, &_rd_foo32->_receivedCall->_responder, cq, &_rd_foo32.get());
            queue_receive(2, &_rd_foo33->_receivedCall->_context, &_rd_foo33->_receivedCall->_request, &_rd_foo33->_receivedCall->_responder, cq, &_rd_foo33.get());
        }

        virtual void foo31(::bond::ext::gRPC::unary_call<::bond::comm::message<Payload>, ::bond::comm::message<void>>) = 0;
        virtual void foo32(::bond::ext::gRPC::unary_call<::bond::comm::message<void>, ::bond::comm::message<Payload>>) = 0;
        virtual void foo33(::bond::ext::gRPC::unary_call<::bond::comm::message<Payload>, ::bond::comm::message<Payload>>) = 0;

    private:
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<Payload>, ::bond::comm::message<void>>> _rd_foo31;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<void>, ::bond::comm::message<Payload>>> _rd_foo32;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<Payload>, ::bond::comm::message<Payload>>> _rd_foo33;
    };

private:
    static const char* method_names[];
};

template <typename TThreadPool>
Foo::Client<TThreadPool>::Client(const std::shared_ptr< ::grpc::ChannelInterface>& channel, std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager, TThreadPool* threadPool)
    : channel_(channel)
    , ioManager_(ioManager)
    , threadPool_(threadPool)
    , rpcmethod_foo31_(method_names[0], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo32_(method_names[1], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo33_(method_names[2], ::grpc::RpcMethod::NORMAL_RPC, channel)
    { }

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asyncfoo31(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, std::function<void(const ::bond::comm::message<void>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<Payload>, ::bond::comm::message<void>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<Payload>, ::bond::comm::message<void>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo31_, context, request);
}

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asyncfoo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message<Payload>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message<Payload>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message<Payload>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo32_, context, request);
}

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asyncfoo33(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, std::function<void(const ::bond::comm::message<Payload>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<Payload>, ::bond::comm::message<Payload>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<Payload>, ::bond::comm::message<Payload>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo33_, context, request);
}


} // namespace tests

