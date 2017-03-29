﻿// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Examples.GrpcPingPong
{
    using System;
    using System.Linq;
    using System.Threading.Tasks;
    using Bond.Grpc;
    using Grpc.Core;

    public static class GrpcPingPong
    {
        const int PingPort = 50051;
        const int DoublePingPort = 15005;

        static Server pingServer;
        static Server doubleServer;

        static Channel pingChannel;
        static Channel doubleChannel;

        public static void Main()
        {
            pingServer = new Server
            {
                Services = { PingPong<PingRequest>.BindService(new PingPongService()) },
                Ports = { new ServerPort("localhost", PingPort, ServerCredentials.Insecure) }
            };
            pingServer.Start();

            doubleServer = new Server
            {
                Services = { DoublePing.BindService(new DoublePingService()) },
                Ports = { new ServerPort("localhost", DoublePingPort, ServerCredentials.Insecure) }
            };
            doubleServer.Start();

            pingChannel = new Channel("localhost", PingPort, ChannelCredentials.Insecure);
            doubleChannel = new Channel("localhost", DoublePingPort, ChannelCredentials.Insecure);

            var tasks = MakeRequestsAndPrintAsync(5);

            Task.WaitAll(tasks);

            Shutdown();

            Console.WriteLine("\n\n\nDone with all requests.");
        }

        private static void Shutdown()
        {
            Task.WaitAll(pingServer.ShutdownAsync(), doubleServer.ShutdownAsync(), pingChannel.ShutdownAsync(), doubleChannel.ShutdownAsync());
        }

        private static Task[] MakeRequestsAndPrintAsync(int numRequests)
        {
            var pingClient = new PingPong<PingRequest>.PingPongClient(pingChannel);
            var doublePingClient = new DoublePing.DoublePingClient(doubleChannel);

            var tasks = new Task[2 * numRequests];

            var rnd = new Random();

            foreach (var requestNum in Enumerable.Range(0, numRequests))
            {
                UInt16 delay = (UInt16)rnd.Next(2000);
                tasks[(2 * requestNum)] = DoPingPong(pingClient, requestNum, "ping" + requestNum.ToString(), delay);

                delay = (UInt16)rnd.Next(2000);
                tasks[(2 * requestNum) + 1] = DoDoublePing(doublePingClient, requestNum, "ping2" + requestNum.ToString(), delay);
            }

            return tasks;
        }

        private static async Task DoPingPong(PingPong<PingRequest>.PingPongClient client, int requestNum, string payload, UInt16 delay)
        {
            var request = new PingRequest { Payload = payload, DelayMilliseconds = delay };
            IMessage<PingResponse> response = await client.PingAsync(request);

            Console.WriteLine($"Request #{requestNum} response: \"{response.Payload.Deserialize().Payload}\". Delay: {delay}");
        }

        private static async Task DoDoublePing(DoublePing.DoublePingClient client, int requestNum, string payload, UInt16 delay)
        {
            var request = new PingRequest { Payload = payload, DelayMilliseconds = delay };

            try
            {
                IMessage<PingResponse> responseUltra = await client.PingUltraAsync(request);
            }
            catch (RpcException e)
            {
                Console.WriteLine($"Error!: #{e.Status.Detail} with code: {e.Status.StatusCode}");
            }

            IMessage<PingResponse> response = await client.PingAsync(request);

            Console.WriteLine($"Request #{requestNum} response: \"{response.Payload.Deserialize().Payload}\". Delay: {delay}");

            IMessage<PingResponse> responsePingNoPayload = await client.PingNoPayloadAsync();
            Console.WriteLine($"Request #{requestNum} response: \"{responsePingNoPayload.Payload.Deserialize().Payload}\".");

            await client.PingVoidAsync();
            client.PongNoResponseAsync(request);
        }
    }
}
