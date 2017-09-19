
#pragma once

#include "generic_service_reflection.h"
#include "generic_service_types.h"

#include <bond/core/bonded.h>
#include <bond/ext/grpc/bond_utils.h>
#include <bond/ext/grpc/client_callback.h>
#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/thread_pool.h>
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

template <typename Payload>
    class Foo final
{
public:
    template <typename TThreadPool>
    class ClientCore
    {
    public:
        ClientCore(
            const std::shared_ptr< ::grpc::ChannelInterface>& channel,
            std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager,
            std::shared_ptr<TThreadPool> threadPool);

        void Asyncfoo31(::std::shared_ptr< ::grpc::ClientContext> context, const ::bond::bonded<Payload>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb);
        void Asyncfoo31(::std::shared_ptr< ::grpc::ClientContext> context, const Payload& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb)
        {
            Asyncfoo31(context, ::bond::bonded<Payload>{request}, cb);
        }
        void Asyncfoo31(const ::bond::bonded<Payload>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb)
        {
            Asyncfoo31(::std::make_shared< ::grpc::ClientContext>(), request, cb);
        }
        void Asyncfoo31(const Payload& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb)
        {
            Asyncfoo31(::std::make_shared< ::grpc::ClientContext>(), ::bond::bonded<Payload>{request}, cb);
        }

        void Asyncfoo32(::std::shared_ptr< ::grpc::ClientContext> context, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< Payload>>)>& cb);
        void Asyncfoo32(const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< Payload>>)>& cb)
        {
            Asyncfoo32(::std::make_shared< ::grpc::ClientContext>(), cb);
        }

        void Asyncfoo33(::std::shared_ptr< ::grpc::ClientContext> context, const ::bond::bonded<Payload>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< Payload>>)>& cb);
        void Asyncfoo33(::std::shared_ptr< ::grpc::ClientContext> context, const Payload& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< Payload>>)>& cb)
        {
            Asyncfoo33(context, ::bond::bonded<Payload>{request}, cb);
        }
        void Asyncfoo33(const ::bond::bonded<Payload>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< Payload>>)>& cb)
        {
            Asyncfoo33(::std::make_shared< ::grpc::ClientContext>(), request, cb);
        }
        void Asyncfoo33(const Payload& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< Payload>>)>& cb)
        {
            Asyncfoo33(::std::make_shared< ::grpc::ClientContext>(), ::bond::bonded<Payload>{request}, cb);
        }

        ClientCore(const ClientCore&) = delete;
        ClientCore& operator=(const ClientCore&) = delete;

        ClientCore(ClientCore&&) = default;
        ClientCore& operator=(ClientCore&&) = default;

    private:
        std::shared_ptr< ::grpc::ChannelInterface> _channel;
        std::shared_ptr< ::bond::ext::gRPC::io_manager> _ioManager;
        std::shared_ptr<TThreadPool> _threadPool;

        const ::grpc::RpcMethod rpcmethod_foo31_;

        const ::grpc::RpcMethod rpcmethod_foo32_;

        const ::grpc::RpcMethod rpcmethod_foo33_;
    };

    using Client = ClientCore< ::bond::ext::gRPC::thread_pool>;

    template <typename TThreadPool>
    class ServiceCore : public ::bond::ext::gRPC::detail::service<TThreadPool>
    {
    public:
        ServiceCore()
        {
            this->AddMethod("/tests.Foo/foo31");
            this->AddMethod("/tests.Foo/foo32");
            this->AddMethod("/tests.Foo/foo33");
        }

        virtual ~ServiceCore() { }
        virtual void start(
            ::grpc::ServerCompletionQueue* cq,
            std::shared_ptr<TThreadPool> tp,
            size_t numRecvData = 1) override
        {
            BOOST_ASSERT(cq);
            BOOST_ASSERT(tp);

            for (size_t i = 0; i < numRecvData; ++i)
            {
                _rd_foo31.emplace_back(
                    this,
                    0,
                    cq,
                    tp,
                    std::bind(&ServiceCore::foo31, this, std::placeholders::_1));
            }
            for (size_t i = 0; i < numRecvData; ++i)
            {
                _rd_foo32.emplace_back(
                    this,
                    1,
                    cq,
                    tp,
                    std::bind(&ServiceCore::foo32, this, std::placeholders::_1));
            }
            for (size_t i = 0; i < numRecvData; ++i)
            {
                _rd_foo33.emplace_back(
                    this,
                    2,
                    cq,
                    tp,
                    std::bind(&ServiceCore::foo33, this, std::placeholders::_1));
            }

            for (auto& theRd : _rd_foo31)
            {
                this->queue_receive(
                    0,
                    &_rd_foo31->_receivedCall->_context,
                    &_rd_foo31->_receivedCall->_request,
                    &_rd_foo31->_receivedCall->_responder,
                    cq,
                    &theRd);
            }
            for (auto& theRd : _rd_foo32)
            {
                this->queue_receive(
                    1,
                    &_rd_foo32->_receivedCall->_context,
                    &_rd_foo32->_receivedCall->_request,
                    &_rd_foo32->_receivedCall->_responder,
                    cq,
                    &theRd);
            }
            for (auto& theRd : _rd_foo33)
            {
                this->queue_receive(
                    2,
                    &_rd_foo33->_receivedCall->_context,
                    &_rd_foo33->_receivedCall->_request,
                    &_rd_foo33->_receivedCall->_responder,
                    cq,
                    &theRd);
            }
        }

        virtual void foo31(::bond::ext::gRPC::unary_call< ::bond::bonded<Payload>, ::bond::Void>) = 0;
        virtual void foo32(::bond::ext::gRPC::unary_call< ::bond::bonded< ::bond::Void>, Payload>) = 0;
        virtual void foo33(::bond::ext::gRPC::unary_call< ::bond::bonded<Payload>, Payload>) = 0;

    private:
        ::std::vector< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded<Payload>, ::bond::Void, TThreadPool>> _rd_foo31;
        ::std::vector< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::bond::Void>, Payload, TThreadPool>> _rd_foo32;
        ::std::vector< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded<Payload>, Payload, TThreadPool>> _rd_foo33;
    };

    using Service = ServiceCore< ::bond::ext::gRPC::thread_pool>;
};

template <typename Payload>
    template <typename TThreadPool>
inline Foo<Payload>::ClientCore<TThreadPool>::ClientCore(
    const std::shared_ptr< ::grpc::ChannelInterface>& channel,
    std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager,
    std::shared_ptr<TThreadPool> threadPool)
    : _channel(channel)
    , _ioManager(ioManager)
    , _threadPool(threadPool)
    , rpcmethod_foo31_("/tests.Foo/foo31", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo32_("/tests.Foo/foo32", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo33_("/tests.Foo/foo33", ::grpc::RpcMethod::NORMAL_RPC, channel)
    { }

template <typename Payload>
    template <typename TThreadPool>
inline void Foo<Payload>::ClientCore<TThreadPool>::Asyncfoo31(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    const ::bond::bonded<Payload>& request,
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb)
{
    
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< Payload, ::bond::Void, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_foo31_, request);
}

template <typename Payload>
    template <typename TThreadPool>
inline void Foo<Payload>::ClientCore<TThreadPool>::Asyncfoo32(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< Payload>>)>& cb)
{
    auto request = ::bond::bonded< ::bond::Void>{ ::bond::Void()};
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, Payload, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_foo32_, request);
}

template <typename Payload>
    template <typename TThreadPool>
inline void Foo<Payload>::ClientCore<TThreadPool>::Asyncfoo33(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    const ::bond::bonded<Payload>& request,
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< Payload>>)>& cb)
{
    
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< Payload, Payload, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_foo33_, request);
}


} // namespace tests

