// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include <stdio.h>

#include <boost/lexical_cast.hpp>
#include <boost/thread/thread.hpp>

// Include auto-generated files
#include "pingpong_types.h"
#include "pingpong_grpc.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4100)
#endif

#include <grpc++/grpc++.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#include <bond/ext/grpc/server.h>
#include <bond/ext/grpc/server_builder.h>
#include <bond/ext/grpc/unary_call.h>

using grpc::Status;
using grpc::StatusCode;

using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;

using namespace PingPongNS;

static int NumRequestsReceived = 0;
static int NumErrorsReceived = 0;

// Logic and data behind the server's behavior.
class PingPongServiceImpl final : public PingPong::Service {

    void Ping(
        bond::ext::gRPC::unary_call<
        bond::comm::message<::PingPongNS::PingRequest>,
        bond::comm::message<::PingPongNS::PingResponse>> call) override
    {
        PingRequest request = call.request().value().Deserialize();

        switch (request.Action)
        {
        case PingAction::Identity:
        {
            printf("Received request \"%s\"\n", request.Payload.c_str());
            fflush(stdout);

            PingResponse response;
            response.Payload = request.Payload;
            NumRequestsReceived++;

            bond::comm::message<PingResponse> resp(response);
            call.Finish(resp, Status::OK);
            break;
        }

        case PingAction::Error:
        {
            printf("Received error request \"%s\"\n", request.Payload.c_str());
            fflush(stdout);

            NumErrorsReceived++;

            Status error(StatusCode::UNIMPLEMENTED, "Application Exception");
            call.FinishWithError(error);
        }

        default:
            printf("Received unknown request \"%s\"\n", request.Payload.c_str());
            fflush(stdout);

            Status error(StatusCode::UNIMPLEMENTED, "Unknown PingAction");
            call.FinishWithError(error);
        }
    }

};

int main()
{
    const std::string server_address("127.0.0.1:" + boost::lexical_cast<std::string>(Port));
    PingPongServiceImpl service;
    bond::ext::gRPC::server_builder builder;
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
    builder.RegisterService(&service);
    std::unique_ptr<bond::ext::gRPC::server> server(builder.BuildAndStart());

    printf("Server ready\n");
    fflush(stdout);

    boost::this_thread::sleep_for(boost::chrono::seconds(3));

    if ((NumRequestsReceived != NumRequests) ||
        (NumErrorsReceived != NumErrors))
    {
        printf("Server failed: Did not receive all expected messages\n");
        fflush(stdout);
        exit(1);
    }

    printf("Server completed\n");
    fflush(stdout);
    return 0;
}
