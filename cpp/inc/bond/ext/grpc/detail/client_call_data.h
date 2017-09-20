// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#ifdef _MSC_VER
    #pragma warning (push)
    #pragma warning (disable: 4100 4702)
#endif

#include <grpc++/grpc++.h>
#include <grpc++/impl/codegen/rpc_method.h>
#include <grpc++/impl/codegen/service_type.h>
#include <grpc++/impl/codegen/status.h>

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <bond/core/bonded.h>
#include <bond/ext/grpc/detail/service.h>
#include <bond/ext/grpc/client_callback.h>
#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/unary_call.h>

#include <boost/assert.hpp>
#include <boost/optional.hpp>

#include <atomic>
#include <functional>
#include <memory>
#include <thread>
#include <stdint.h>

namespace bond { namespace ext { namespace gRPC { namespace detail {

                extern std::atomic<size_t> totalDispatched;
                extern std::atomic<size_t> okDispatched;
                extern std::atomic<size_t> okRun;
                extern std::atomic<size_t> errDispatched;

/// @brief Implementation class that hold the state associated with
/// outgoing unary calls.
template <typename TRequest, typename TResponse, typename TThreadPool>
struct client_unary_call_data
    : std::enable_shared_from_this<client_unary_call_data<TRequest, TResponse, TThreadPool>>,
      io_manager_tag
{
    /// The type of the user-defined callback that will be invoked for the
    /// response.
    typedef std::function<void(std::shared_ptr<unary_call_result<TResponse>>)> CallbackType;

    /// The channel to send the request on.
    std::shared_ptr<grpc::ChannelInterface> _channel;
    /// The io_manager to use for both sending and receiving.
    std::shared_ptr<io_manager> _ioManager;
    /// The thread pool in which to invoke the callback.
    std::shared_ptr<TThreadPool> _threadPool;
    /// A response reader.
    std::unique_ptr<grpc::ClientAsyncResponseReader<bond::bonded<TResponse>>> _responseReader;
    /// The arguments to pass back into the client callback. Also doubles as
    /// storage for the response, status, and context.
    unary_call_result<TResponse> _callbackArgs;
    /// A pointer to ourselves used to keep us alive while waiting to receive
    /// the response.
    std::shared_ptr<client_unary_call_data> _self;
    /// The user code to invoke when a response is received.
    CallbackType _cb;

    client_unary_call_data(
        std::shared_ptr<grpc::ChannelInterface> channel,
        std::shared_ptr<io_manager> ioManager,
        std::shared_ptr<TThreadPool> threadPool,
        std::shared_ptr<grpc::ClientContext> context,
        CallbackType cb = {})
        : _channel(std::move(channel)),
          _ioManager(std::move(ioManager)),
          _threadPool(std::move(threadPool)),
          _responseReader(),
          _callbackArgs(context),
          _self(),
          _cb(cb)
    {
        BOOST_ASSERT(_channel);
        BOOST_ASSERT(_ioManager);
        BOOST_ASSERT(_threadPool);
        BOOST_ASSERT(context);
    }

    /// @brief Initiates the client request and wires up completion
    /// notification.
    void dispatch(
        const grpc::RpcMethod& method,
        const bond::bonded<TRequest>& request)
    {
        _responseReader = std::unique_ptr<grpc::ClientAsyncResponseReader<bond::bonded<TResponse>>>(
            ::grpc::ClientAsyncResponseReader<bond::bonded<TResponse>>::Create(
                _channel.get(),
                _ioManager->cq(),
                method,
                _callbackArgs.context.get(),
                request));

        _self = this->shared_from_this();

        _responseReader->Finish(
            &_callbackArgs.response,
            &_callbackArgs.status,
            static_cast<void*>(this));
    }

    /// @brief Invoked after the response has been received.
    void invoke(bool ok) override
    {
        if (ok && _cb)
        {
            _threadPool->schedule([this]()
            {
                // pass a shared_ptr to unary_call_result, but that
                // participates in shared ownership of the containing
                // client_unary_call_data
                _cb(std::shared_ptr<unary_call_result<TResponse>>(_self, &_callbackArgs));
                clean_up_after_receive();

                ++okRun;
            });

            ++okDispatched;
        }
        else
        {
            clean_up_after_receive();
            ++errDispatched;
        }

        ++totalDispatched;
    }

    /// @brief Cleans up resources that are no longer needed after receiving
    /// the response.
    ///
    /// The callback may copy the unary_call_result shared_ptr that is gets,
    /// but we don't need these members anymore.
    void clean_up_after_receive()
    {
        _channel.reset();
        _ioManager.reset();
        _threadPool.reset();
        _responseReader.reset();
        _self.reset();
    }
};

} } } } //namespace bond::ext::gRPC::detail
