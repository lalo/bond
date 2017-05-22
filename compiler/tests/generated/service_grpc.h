
#pragma once

#include "service_reflection.h"
#include "service_types.h"
#include "basic_types_grpc.h"
#include "namespace_basic_types_grpc.h"
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

        /* TODO stub implementation (public) for event foo11 */

        /* TODO stub implementation (public) for event foo12 */

        /* TODO stub implementation (public) for event foo12_impl */

        /* TODO stub implementation (public) for event foo13 */

        /* TODO stub implementation (public) for event foo14 */

        /* TODO stub implementation (public) for event foo15 */

        void Asyncfoo21(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message<void>&, const ::grpc::Status&)> cb);

        void Asyncfoo22(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message<void>&, const ::grpc::Status&)> cb);

        void Asyncfoo23(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, std::function<void(const ::bond::comm::message<void>&, const ::grpc::Status&)> cb);

        void Asyncfoo24(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, std::function<void(const ::bond::comm::message<void>&, const ::grpc::Status&)> cb);

        void Asyncfoo31(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb);

        void Asyncfoo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb);

        void Asyncfoo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb);

        void Async_rd_foo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb);

        void Asyncfoo34(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb);

        void Asyncfoo41(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message< ::tests::dummy>&, const ::grpc::Status&)> cb);

        void Asyncfoo42(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message< ::tests::dummy>&, const ::grpc::Status&)> cb);

        void Asyncfoo43(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, std::function<void(const ::bond::comm::message< ::tests::dummy>&, const ::grpc::Status&)> cb);

        void Asyncfoo44(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, std::function<void(const ::bond::comm::message< ::tests::dummy>&, const ::grpc::Status&)> cb);

        void Asynccq(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb);

        Client(const Client&) = delete;
        Client& operator=(const Client&) = delete;

        Client(Client&&) = default;
        Client& operator=(Client&&) = default;

    private:
        std::shared_ptr< ::grpc::ChannelInterface> channel_;
        std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager_;
        TThreadPool* threadPool_;

        /* TODO stub implementation (private) for event foo11 */

        /* TODO stub implementation (private) for event foo12 */

        /* TODO stub implementation (private) for event foo12_impl */

        /* TODO stub implementation (private) for event foo13 */

        /* TODO stub implementation (private) for event foo14 */

        /* TODO stub implementation (private) for event foo15 */

        const ::grpc::RpcMethod rpcmethod_foo21_;

        const ::grpc::RpcMethod rpcmethod_foo22_;

        const ::grpc::RpcMethod rpcmethod_foo23_;

        const ::grpc::RpcMethod rpcmethod_foo24_;

        const ::grpc::RpcMethod rpcmethod_foo31_;

        const ::grpc::RpcMethod rpcmethod_foo32_;

        const ::grpc::RpcMethod rpcmethod_foo33_;

        const ::grpc::RpcMethod rpcmethod__rd_foo33_;

        const ::grpc::RpcMethod rpcmethod_foo34_;

        const ::grpc::RpcMethod rpcmethod_foo41_;

        const ::grpc::RpcMethod rpcmethod_foo42_;

        const ::grpc::RpcMethod rpcmethod_foo43_;

        const ::grpc::RpcMethod rpcmethod_foo44_;

        const ::grpc::RpcMethod rpcmethod_cq_;
    };

    class Service : public ::bond::ext::gRPC::detail::service
    {
    public:
        Service()
        {
            AddMethod("/tests.Foo/foo11");
            AddMethod("/tests.Foo/foo12");
            AddMethod("/tests.Foo/foo12_impl");
            AddMethod("/tests.Foo/foo13");
            AddMethod("/tests.Foo/foo14");
            AddMethod("/tests.Foo/foo15");
            AddMethod("/tests.Foo/foo21");
            AddMethod("/tests.Foo/foo22");
            AddMethod("/tests.Foo/foo23");
            AddMethod("/tests.Foo/foo24");
            AddMethod("/tests.Foo/foo31");
            AddMethod("/tests.Foo/foo32");
            AddMethod("/tests.Foo/foo33");
            AddMethod("/tests.Foo/_rd_foo33");
            AddMethod("/tests.Foo/foo34");
            AddMethod("/tests.Foo/foo41");
            AddMethod("/tests.Foo/foo42");
            AddMethod("/tests.Foo/foo43");
            AddMethod("/tests.Foo/foo44");
            AddMethod("/tests.Foo/cq");
        }

        virtual ~Service() { }
        virtual void start(::grpc::ServerCompletionQueue* cq0) override
        {
            BOOST_ASSERT(cq0);

            /* TODO: init for event foo11 */
            /* TODO: init for event foo12 */
            /* TODO: init for event foo12_impl */
            /* TODO: init for event foo13 */
            /* TODO: init for event foo14 */
            /* TODO: init for event foo15 */
            _rd_foo21.emplace(this, 6, cq0, std::bind(&Service::foo21, this, std::placeholders::_1));
            _rd_foo22.emplace(this, 7, cq0, std::bind(&Service::foo22, this, std::placeholders::_1));
            _rd_foo23.emplace(this, 8, cq0, std::bind(&Service::foo23, this, std::placeholders::_1));
            _rd_foo24.emplace(this, 9, cq0, std::bind(&Service::foo24, this, std::placeholders::_1));
            _rd_foo31.emplace(this, 10, cq0, std::bind(&Service::foo31, this, std::placeholders::_1));
            _rd_foo32.emplace(this, 11, cq0, std::bind(&Service::foo32, this, std::placeholders::_1));
            _rd_foo330.emplace(this, 12, cq0, std::bind(&Service::foo33, this, std::placeholders::_1));
            _rd__rd_foo33.emplace(this, 13, cq0, std::bind(&Service::_rd_foo33, this, std::placeholders::_1));
            _rd_foo34.emplace(this, 14, cq0, std::bind(&Service::foo34, this, std::placeholders::_1));
            _rd_foo41.emplace(this, 15, cq0, std::bind(&Service::foo41, this, std::placeholders::_1));
            _rd_foo42.emplace(this, 16, cq0, std::bind(&Service::foo42, this, std::placeholders::_1));
            _rd_foo43.emplace(this, 17, cq0, std::bind(&Service::foo43, this, std::placeholders::_1));
            _rd_foo44.emplace(this, 18, cq0, std::bind(&Service::foo44, this, std::placeholders::_1));
            _rd_cq.emplace(this, 19, cq0, std::bind(&Service::cq, this, std::placeholders::_1));

            /* TODO: queue event foo11 */
            /* TODO: queue event foo12 */
            /* TODO: queue event foo12_impl */
            /* TODO: queue event foo13 */
            /* TODO: queue event foo14 */
            /* TODO: queue event foo15 */
            queue_receive(6, &_rd_foo21->_receivedCall->_context, &_rd_foo21->_receivedCall->_request, &_rd_foo21->_receivedCall->_responder, cq0, &_rd_foo21.get());
            queue_receive(7, &_rd_foo22->_receivedCall->_context, &_rd_foo22->_receivedCall->_request, &_rd_foo22->_receivedCall->_responder, cq0, &_rd_foo22.get());
            queue_receive(8, &_rd_foo23->_receivedCall->_context, &_rd_foo23->_receivedCall->_request, &_rd_foo23->_receivedCall->_responder, cq0, &_rd_foo23.get());
            queue_receive(9, &_rd_foo24->_receivedCall->_context, &_rd_foo24->_receivedCall->_request, &_rd_foo24->_receivedCall->_responder, cq0, &_rd_foo24.get());
            queue_receive(10, &_rd_foo31->_receivedCall->_context, &_rd_foo31->_receivedCall->_request, &_rd_foo31->_receivedCall->_responder, cq0, &_rd_foo31.get());
            queue_receive(11, &_rd_foo32->_receivedCall->_context, &_rd_foo32->_receivedCall->_request, &_rd_foo32->_receivedCall->_responder, cq0, &_rd_foo32.get());
            queue_receive(12, &_rd_foo330->_receivedCall->_context, &_rd_foo330->_receivedCall->_request, &_rd_foo330->_receivedCall->_responder, cq0, &_rd_foo330.get());
            queue_receive(13, &_rd__rd_foo33->_receivedCall->_context, &_rd__rd_foo33->_receivedCall->_request, &_rd__rd_foo33->_receivedCall->_responder, cq0, &_rd__rd_foo33.get());
            queue_receive(14, &_rd_foo34->_receivedCall->_context, &_rd_foo34->_receivedCall->_request, &_rd_foo34->_receivedCall->_responder, cq0, &_rd_foo34.get());
            queue_receive(15, &_rd_foo41->_receivedCall->_context, &_rd_foo41->_receivedCall->_request, &_rd_foo41->_receivedCall->_responder, cq0, &_rd_foo41.get());
            queue_receive(16, &_rd_foo42->_receivedCall->_context, &_rd_foo42->_receivedCall->_request, &_rd_foo42->_receivedCall->_responder, cq0, &_rd_foo42.get());
            queue_receive(17, &_rd_foo43->_receivedCall->_context, &_rd_foo43->_receivedCall->_request, &_rd_foo43->_receivedCall->_responder, cq0, &_rd_foo43.get());
            queue_receive(18, &_rd_foo44->_receivedCall->_context, &_rd_foo44->_receivedCall->_request, &_rd_foo44->_receivedCall->_responder, cq0, &_rd_foo44.get());
            queue_receive(19, &_rd_cq->_receivedCall->_context, &_rd_cq->_receivedCall->_request, &_rd_cq->_receivedCall->_responder, cq0, &_rd_cq.get());
        }

        /* TODO: abstract method for event foo11 */
        /* TODO: abstract method for event foo12 */
        /* TODO: abstract method for event foo12_impl */
        /* TODO: abstract method for event foo13 */
        /* TODO: abstract method for event foo14 */
        /* TODO: abstract method for event foo15 */
        virtual void foo21(::bond::ext::gRPC::unary_call<::bond::comm::message<void>, ::bond::comm::message<void>>) = 0;
        virtual void foo22(::bond::ext::gRPC::unary_call<::bond::comm::message<void>, ::bond::comm::message<void>>) = 0;
        virtual void foo23(::bond::ext::gRPC::unary_call<::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message<void>>) = 0;
        virtual void foo24(::bond::ext::gRPC::unary_call<::bond::comm::message< ::tests::dummy>, ::bond::comm::message<void>>) = 0;
        virtual void foo31(::bond::ext::gRPC::unary_call<::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>>) = 0;
        virtual void foo32(::bond::ext::gRPC::unary_call<::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>>) = 0;
        virtual void foo33(::bond::ext::gRPC::unary_call<::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::BasicTypes>>) = 0;
        virtual void _rd_foo33(::bond::ext::gRPC::unary_call<::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::BasicTypes>>) = 0;
        virtual void foo34(::bond::ext::gRPC::unary_call<::bond::comm::message< ::tests::dummy>, ::bond::comm::message< ::tests::BasicTypes>>) = 0;
        virtual void foo41(::bond::ext::gRPC::unary_call<::bond::comm::message<void>, ::bond::comm::message< ::tests::dummy>>) = 0;
        virtual void foo42(::bond::ext::gRPC::unary_call<::bond::comm::message<void>, ::bond::comm::message< ::tests::dummy>>) = 0;
        virtual void foo43(::bond::ext::gRPC::unary_call<::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::dummy>>) = 0;
        virtual void foo44(::bond::ext::gRPC::unary_call<::bond::comm::message< ::tests::dummy>, ::bond::comm::message< ::tests::dummy>>) = 0;
        virtual void cq(::bond::ext::gRPC::unary_call<::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>>) = 0;

    private:
        /* TODO: receive data for event foo11 */
        /* TODO: receive data for event foo12 */
        /* TODO: receive data for event foo12_impl */
        /* TODO: receive data for event foo13 */
        /* TODO: receive data for event foo14 */
        /* TODO: receive data for event foo15 */
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<void>, ::bond::comm::message<void>>> _rd_foo21;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<void>, ::bond::comm::message<void>>> _rd_foo22;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message<void>>> _rd_foo23;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message< ::tests::dummy>, ::bond::comm::message<void>>> _rd_foo24;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>>> _rd_foo31;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>>> _rd_foo32;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::BasicTypes>>> _rd_foo330;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::BasicTypes>>> _rd__rd_foo33;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message< ::tests::dummy>, ::bond::comm::message< ::tests::BasicTypes>>> _rd_foo34;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<void>, ::bond::comm::message< ::tests::dummy>>> _rd_foo41;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<void>, ::bond::comm::message< ::tests::dummy>>> _rd_foo42;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::dummy>>> _rd_foo43;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message< ::tests::dummy>, ::bond::comm::message< ::tests::dummy>>> _rd_foo44;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>>> _rd_cq;
    };

private:
    static const char* method_names[];
};

template <typename TThreadPool>
Foo::Client<TThreadPool>::Client(const std::shared_ptr< ::grpc::ChannelInterface>& channel, std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager, TThreadPool* threadPool)
    : channel_(channel)
    , ioManager_(ioManager)
    , threadPool_(threadPool)
    /* TODO stub ctor initialization for event foo11 */
    /* TODO stub ctor initialization for event foo12 */
    /* TODO stub ctor initialization for event foo12_impl */
    /* TODO stub ctor initialization for event foo13 */
    /* TODO stub ctor initialization for event foo14 */
    /* TODO stub ctor initialization for event foo15 */
    , rpcmethod_foo21_(method_names[6], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo22_(method_names[7], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo23_(method_names[8], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo24_(method_names[9], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo31_(method_names[10], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo32_(method_names[11], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo33_(method_names[12], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod__rd_foo33_(method_names[13], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo34_(method_names[14], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo41_(method_names[15], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo42_(method_names[16], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo43_(method_names[17], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo44_(method_names[18], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_cq_(method_names[19], ::grpc::RpcMethod::NORMAL_RPC, channel)
    { }

/* TODO: stub implementation for event foo11 */

/* TODO: stub implementation for event foo12 */

/* TODO: stub implementation for event foo12_impl */

/* TODO: stub implementation for event foo13 */

/* TODO: stub implementation for event foo14 */

/* TODO: stub implementation for event foo15 */

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asyncfoo21(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message<void>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message<void>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message<void>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo21_, context, request);
}

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asyncfoo22(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message<void>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message<void>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message<void>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo22_, context, request);
}

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asyncfoo23(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, std::function<void(const ::bond::comm::message<void>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message<void>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message<void>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo23_, context, request);
}

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asyncfoo24(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, std::function<void(const ::bond::comm::message<void>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::dummy>, ::bond::comm::message<void>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::dummy>, ::bond::comm::message<void>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo24_, context, request);
}

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asyncfoo31(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo31_, context, request);
}

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asyncfoo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo32_, context, request);
}

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asyncfoo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::BasicTypes>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::BasicTypes>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo33_, context, request);
}

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Async_rd_foo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::BasicTypes>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::BasicTypes>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod__rd_foo33_, context, request);
}

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asyncfoo34(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::dummy>, ::bond::comm::message< ::tests::BasicTypes>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::dummy>, ::bond::comm::message< ::tests::BasicTypes>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo34_, context, request);
}

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asyncfoo41(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message< ::tests::dummy>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::dummy>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::dummy>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo41_, context, request);
}

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asyncfoo42(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message< ::tests::dummy>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::dummy>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::dummy>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo42_, context, request);
}

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asyncfoo43(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, std::function<void(const ::bond::comm::message< ::tests::dummy>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::dummy>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::dummy>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo43_, context, request);
}

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asyncfoo44(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, std::function<void(const ::bond::comm::message< ::tests::dummy>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::dummy>, ::bond::comm::message< ::tests::dummy>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::dummy>, ::bond::comm::message< ::tests::dummy>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo44_, context, request);
}

template <typename TThreadPool>
void Foo::Client<TThreadPool>::Asynccq(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>, TThreadPool >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>, TThreadPool >(cb, threadPool_);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_cq_, context, request);
}


} // namespace tests

