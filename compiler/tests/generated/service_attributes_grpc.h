
#pragma once

#include "service_attributes_reflection.h"
#include "service_attributes_types.h"

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

        void Asyncfoo(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, std::function<void(const ::bond::comm::message< ::tests::Result>&, const ::grpc::Status&)> cb);

        Client(const Client&) = delete;
        Client& operator=(const Client&) = delete;

        Client(Client&&) = default;
        Client& operator=(Client&&) = default;

    private:
        std::shared_ptr< ::grpc::ChannelInterface> channel_;
        std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager_;
        TThreadPool* threadPool_;

        const ::grpc::RpcMethod rpcmethod_foo_;
    };

    class Service : public ::bond::ext::gRPC::detail::service
    {
    public:
        Service()
        {
            AddMethod("/tests.Foo/foo");
        }

        virtual ~Service() { }
        virtual void start(::grpc::ServerCompletionQueue* cq) override
        {
            BOOST_ASSERT(cq);

            _rd_foo.emplace(this, 0, cq, std::bind(&Service::foo, this, std::placeholders::_1));

            queue_receive(0, &_rd_foo->_receivedCall->_context, &_rd_foo->_receivedCall->_request, &_rd_foo->_receivedCall->_responder, cq, &_rd_foo.get());
        }

        virtual void foo(::bond::ext::gRPC::unary_call<::bond::comm::message< ::tests::Param>, ::bond::comm::message< ::tests::Result>>) = 0;

    private:
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message< ::tests::Param>, ::bond::comm::message< ::tests::Result>>> _rd_foo;
    };

private:
    static const char* method_names[];
};

template <typename TThreadPool>
Foo::Client<TThreadPool>::Client(const std::shared_ptr< ::grpc::ChannelInterface>& channel, std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager, TThreadPool* threadPool)
    : channel_(channel)
    , ioManager_(ioManager)
    , threadPool_(threadPool)
    , rpcmethod_foo_(method_names[0], ::grpc::RpcMethod::NORMAL_RPC, channel)
    { }

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asyncfoo(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, std::function<void(const ::bond::comm::message< ::tests::Result>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::Param>, ::bond::comm::message< ::tests::Result>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::Param>, ::bond::comm::message< ::tests::Result>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo_, context, request);
}


} // namespace tests

