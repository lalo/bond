// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace PingPongServer
{
    using System;
    using System.Threading;
    using System.Threading.Tasks;

    using Bond.Grpc;
    using Grpc.Core;

    using PingPongNS;

    public class PingPongService : PingPong.PingPongBase
    {
        static Server pingServer;

        static int NumRequestsReceived = 0;
        static int NumErrorsReceived = 0;

        public override Task<IMessage<PingResponse>> Ping(IMessage<PingRequest> param, ServerCallContext context)
        {
            PingRequest request = param.Payload.Deserialize();

            IMessage<PingResponse> message = null;

            switch (request.Action)
            {
                case PingAction.Identity:
                    Console.Out.WriteLine($"Received mirror request \"{request.Payload}\"");
                    Console.Out.Flush();

                    var response = new PingResponse { Payload = request.Payload };
                    message = Message.From(response);
                    NumRequestsReceived++;
                    break;

                case PingAction.Error:
                    Console.Out.WriteLine($"Received error request \"{request.Payload}\"");
                    Console.Out.Flush();
                    NumErrorsReceived++;
                    throw new ApplicationException("Application Exception");

                default:
                    throw new NotImplementedException("Unknown PingAction");
            }

            return Task.FromResult(message);
        }

        private static void Setup()
        {
            pingServer = new Server
            {
                Services =
                {
                    PingPong.BindService(new PingPongService()),
                },
                Ports = { new ServerPort("localhost", (int)PingConstants.Port, ServerCredentials.Insecure) }
            };
            pingServer.Start();
        }

        static void Main(string[] args)
        {
            Setup();

            Console.Out.WriteLine("Server ready");
            Console.Out.Flush();

            Thread.Sleep(3000);

            if ((NumRequestsReceived != (int)PingConstants.NumRequests) ||
                (NumErrorsReceived != (int)PingConstants.NumErrors))
            {
                Console.Out.WriteLine("Server failed: Did not receive all expected messages");
                Console.Out.Flush();
                return;
            }

            Console.Out.WriteLine("Server completed");
            Console.Out.Flush();
        }
    }
}
