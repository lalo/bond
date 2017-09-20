#include "pingpong_grpc.h"
#include "pingpong_types.h"

// event.h needed for test purposes
#include <bond/ext/detail/event.h>
#include <bond/ext/detail/countdown_event.h>

#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/server.h>
#include <bond/ext/grpc/server_builder.h>
#include <bond/ext/grpc/thread_pool.h>
#include <bond/ext/grpc/unary_call.h>
#include <bond/ext/grpc/wait_callback.h>
#include <bond/ext/grpc/detail/client_call_data.h>

#include <atomic>
#include <chrono>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <memory>
#include <string>

namespace bond { namespace ext { namespace gRPC { namespace detail {

extern std::atomic<size_t> totalDispatched {};
extern std::atomic<size_t> okDispatched {};
extern std::atomic<size_t> okRun {};
extern std::atomic<size_t> errDispatched {};

}}}}

#pragma warning(disable: 4505)
#include <boost/program_options.hpp>
#pragma warning(disable: 4505)

namespace po = boost::program_options;

using grpc::Channel;
using grpc::ServerBuilder;
using grpc::Status;
using grpc::StatusCode;

using bond::ext::detail::event;
using bond::ext::detail::countdown_event;
using bond::ext::gRPC::io_manager;
using bond::ext::gRPC::wait_callback;

using namespace pingpong;

static const char* metadata_key = "metadata-key";

class DoublePingServiceImpl final : public DoublePing::Service
{
public:
    event pingNoResponse_event;
    std::atomic<int> pos;

private:
    void Ping(
        bond::ext::gRPC::unary_call<
            bond::bonded<PingRequest>,
            PingReply> call) override
    {
        std::string key("req-id");
        auto clientMetadata = call.context().client_metadata();
        std::string reqId("unknown");
        auto reqIdIter = clientMetadata.find(grpc::string_ref(key));
        if (reqIdIter != std::end(clientMetadata))
        {
            reqId.assign(reqIdIter->second.cbegin(), reqIdIter->second.cend());;
        }

        pos++;
        std::cerr << "receive:" << (pos.load()) << std::endl;
        // PingRequest request = call.request().Deserialize();

        PingReply reply;
        reply.message = "ping " ;//+ request.name;

        call.context().AddInitialMetadata(key, reqId);
        call.Finish(reply);
    }
};

static event shutdownEvent {};

void run_server(std::string ip, size_t serverRecvData)
{
    auto threadPool = std::make_shared<bond::ext::gRPC::thread_pool>();

    DoublePingServiceImpl double_ping_service;

    bond::ext::gRPC::server_builder builder;
    builder.SetThreadPool(threadPool);
    const std::string server_address(ip + ":9143");
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
    builder.RegisterService(&double_ping_service);
    std::unique_ptr<bond::ext::gRPC::server> server(builder.BuildAndStart(serverRecvData));

    shutdownEvent.wait();
}

int setup_server(std::string ip, size_t serverRecvData, int seconds)
{
    std::thread t1(run_server, ip, serverRecvData);

    auto start = std::chrono::high_resolution_clock::now();
    std::this_thread::sleep_for(std::chrono::seconds(seconds/2));
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double, std::milli> elapsed = end - start;
    std::cout << "Waited " << elapsed.count() << " ms\n";
    shutdownEvent.set();
    t1.join();
    start = std::chrono::high_resolution_clock::now();
    std::this_thread::sleep_for(std::chrono::seconds(seconds/2));
    end = std::chrono::high_resolution_clock::now();
    elapsed = end - start;
    std::cout << "Waited " << elapsed.count() << " ms\n";

    return 0;
}

int setup_client(std::string ip, int seconds, int quantity, int sizeBytes, int threads)
{
    // auto ioManager = std::make_shared<io_manager>();
    // auto threadPool = std::make_shared<bond::ext::gRPC::thread_pool>();
    //
    // DoublePing::Client doublePing(
    //     grpc::CreateChannel(ip + ":9143", grpc::InsecureChannelCredentials()),
    //     ioManager,
    //     threadPool);

    auto start = std::chrono::high_resolution_clock::now();
    std::this_thread::sleep_for(std::chrono::seconds(seconds));
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double, std::milli> elapsed = end - start;
    std::cout << "Waited " << elapsed.count() << " ms\n";

    // var to keep track of responses that are failures across all threads
    std::atomic<int> neg(0);
    std::cout << std::boolalpha
              << "std::atomic<int> is lock free? "
              << std::atomic_is_lock_free(&neg) << '\n';

    auto threadPool = std::make_shared<bond::ext::gRPC::thread_pool>();
    // auto channel = grpc::CreateChannel(ip + ":9143", grpc::InsecureChannelCredentials());
    // auto ioManager = std::make_shared<io_manager>();

    PingRequest request;
    request.name = "ping";
    void* content = calloc(sizeBytes,1);
    request.size = bond::blob(content, sizeBytes);

    bond::OutputBuffer output;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(output);

    Serialize(request, writer);
    bond::blob data = output.GetBuffer();

    bond::CompactBinaryReader<bond::InputBuffer> reader(data);
    bond::bonded<PingRequest> bondedRequest(reader);

    std::atomic<size_t> reqId{ 0 };

    auto f_pinger = [bondedRequest, &quantity, &threads, &ip/* ,  &doublePing */, &neg/*, &sizeBytes*//* ,  &ioManager */,  &threadPool, &reqId](/*const std::shared_ptr< ::grpc::ChannelInterface>& channel*/)
    {
        auto ioManager = std::make_shared<io_manager>();
        auto channel = grpc::CreateChannel(ip + ":9143", grpc::InsecureChannelCredentials());

        DoublePing::Client doublePing(
            channel,
            ioManager,
            threadPool);

        // var to keep track of responses that came back per thread
        //std::atomic<int> pos(0);

        countdown_event test(quantity / threads);

        auto f_count = [/*&pos,*/ &neg, &test](std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::pingpong::PingReply>> res)
        {
            if((res->status.ok()))
            {
            }
            else
            {
                //DebugBreak();
                neg++;
                std::cerr << res->status.error_code() << ":" << res->status.error_message() << std::endl;
                std::cerr << res->status.error_details() << std::endl;
            }

            //pos++;
            test.set();
        };

        for (int i = 0; i < quantity/threads; i++) {
            std::shared_ptr< grpc::ClientContext> context(new grpc::ClientContext());
            size_t myReqId = ++reqId;
            context->AddMetadata("req-id", std::to_string(myReqId));
            context->set_compression_algorithm(GRPC_COMPRESS_GZIP);

            doublePing.AsyncPing(context, bondedRequest, f_count);
        }

        // busy loop until pos counter reaches the quantity per thread
        while (!test.wait_for(std::chrono::seconds(5)))
        {
            std::cerr
            << "Timeout waiting for test to finish."
            << " Total dispatched: " << bond::ext::gRPC::detail::totalDispatched
            << " OK dispatched: " << bond::ext::gRPC::detail::okDispatched
            << " ERR dispatched: " << bond::ext::gRPC::detail::errDispatched
            << " OK run: " << bond::ext::gRPC::detail::okRun
            << std::endl;
        }
        /*
        while (true)
        {
            if (pos.load() >= quantity/threads)
                return;
        }
        */
    };

    std::thread* thread_arr = new std::thread[threads];

    start = std::chrono::high_resolution_clock::now();

    for (int i = 0; i < threads; i++) {
        thread_arr[i] = std::thread(f_pinger);
    }

    for (int i = 0; i < threads; i++) {
        thread_arr[i].join();
    }

    end = std::chrono::high_resolution_clock::now();
    elapsed = end - start;

    std::cout << elapsed.count() << std::endl;
    std::cout << "errores:" << (neg.load()) << std::endl;
    std::cout << "buenos:" << (quantity - neg.load()) << std::endl;
    std::cout << (quantity - neg.load())/(elapsed.count()/1000) << " p/s";

    return 0;
}

int main(int ac, char* av[])
{
    std::string server_ip = "";
    bool client = false;
    int duration = 0;
    int quantity = 0;
    int threads = 0;
    int sizeBytes = 0;
    size_t serverRecvData = 1;

    try {
        po::options_description desc("Allowed options");
        desc.add_options()
            ("help", "produce help message")
            ("server,s", po::value(&server_ip)->required(), "server IP")
            ("duration,d", po::value(&duration)->required(), "time of duration in seconds")
            ("client,c", po::bool_switch(&client), "client mode")
            ("quantity,q", po::value(&quantity), "quantity of pings")
            ("threads,t", po::value(&threads), "quantity of main threads that divide work")
            ("sizeBytes,b", po::value(&sizeBytes), "payload in bytes")
            ("serverRecvData,r", po::value(&serverRecvData), "number of server-side recv data structs per method")
            ;

        po::variables_map vm;
        po::store(po::parse_command_line(ac, av, desc), vm);
        po::notify(vm);

        if (vm.count("help")) {
            std::cout << desc << "\n";
            return 0;
        }

        if (server_ip.empty()) {
            std::cout << "server IP was not set.\n";
        }
    }
    catch (std::exception& e) {
        std::cerr << "error: " << e.what() << "\n";
        return 1;
    }
    catch (...) {
        std::cerr << "Exception of unknown type!\n";
    }

    if (client)
        return setup_client(server_ip, duration, quantity, sizeBytes, threads);
    else
        return setup_server(server_ip, serverRecvData, duration);

    return 0;
}
