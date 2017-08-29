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

#include <atomic>
#include <chrono>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <memory>
#include <string>

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

private:
    void Ping(
        bond::ext::gRPC::unary_call<
            bond::bonded<PingRequest>,
            PingReply> call) override
    {
        // PingRequest request = call.request().Deserialize();

        PingReply reply;
        reply.message = "ping " ;//+ request.name;

        call.Finish(reply);
    }
};

volatile bool run = true;

void run_server(std::string ip)
{
    auto threadPool = std::make_shared<bond::ext::gRPC::thread_pool>();

    DoublePingServiceImpl double_ping_service;

    bond::ext::gRPC::server_builder builder;
    builder.SetThreadPool(threadPool);
    const std::string server_address(ip + ":9143");
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
    builder.RegisterService(&double_ping_service);
    std::unique_ptr<bond::ext::gRPC::server> server(builder.BuildAndStart());

    while (run) {}
}

int setup_server(std::string ip, int seconds)
{
    std::thread t1(run_server, ip);

    auto start = std::chrono::high_resolution_clock::now();
    std::this_thread::sleep_for(std::chrono::seconds(seconds/2));
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double, std::milli> elapsed = end - start;
    std::cout << "Waited " << elapsed.count() << " ms\n";
    run = false;
    t1.join();
    start = std::chrono::high_resolution_clock::now();
    std::this_thread::sleep_for(std::chrono::seconds(seconds/2));
    end = std::chrono::high_resolution_clock::now();
    elapsed = end - start;
    std::cout << "Waited " << elapsed.count() << " ms\n";
    //std::terminate();

    return 0;
}

int setup_client(std::string ip, int seconds, int quantity, int sizeBytes)
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

    std::atomic<int> neg(0);
    std::cout << std::boolalpha
              << "std::atomic<int> is lock free? "
              << std::atomic_is_lock_free(&neg) << '\n';

    auto f_pinger = [&quantity, &ip,/*  &doublePing, */ &neg, &sizeBytes]()
    {
        auto ioManager = std::make_shared<io_manager>();
        auto threadPool = std::make_shared<bond::ext::gRPC::thread_pool>();

        DoublePing::Client doublePing(
            grpc::CreateChannel(ip + ":9143", grpc::InsecureChannelCredentials()),
            ioManager,
            threadPool);

        std::atomic<int> pos(0);

        // countdown_event co(quantity/8);

        auto f_count = [&pos, &neg /*, &co*/](std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::pingpong::PingReply>> res)
        {
            pos++;
            if((res->status.ok()))
            {
            }
            else
            {
                //DebugBreak();
                neg++;
                std::cerr << res->status.error_code() << ":" << res->status.error_message() << std::endl;
            }
            // co.set();
        };

        for (int i = 0; i < quantity/4; i++) {
            // if ( i == quantity/8 )
            //     std::this_thread::sleep_for(std::chrono::milliseconds(200));


            // A bonded object can also be used for the request. Here we use a
            // bonded object backed by an instance of PingRequest, but we could
            // also use one backed by a reader.
            std::shared_ptr< grpc::ClientContext> context(new grpc::ClientContext());
            context->set_compression_algorithm(GRPC_COMPRESS_GZIP);

            PingRequest request;
            request.name = "ping";
            void* content = calloc(sizeBytes,1);
            request.size = bond::blob(content, sizeBytes);

            doublePing.AsyncPing(context, request, f_count);
            // assertResponseReceived(cb, __LINE__);
            // assertResponseContents(cb, __LINE__);
        }

        /*
        bool countdownSet = co.wait_for(std::chrono::minutes(10));
        if (countdownSet)
        {
        }
        */
        while (true)
        {
            if (pos.load() >= quantity/4)
                return;
        }
    };

    start = std::chrono::high_resolution_clock::now();
    std::thread t1(f_pinger);
    // std::thread t2(f_pinger);
    std::this_thread::sleep_for(std::chrono::milliseconds(200));
    std::thread t3(f_pinger);
    // std::thread t4(f_pinger);
    std::this_thread::sleep_for(std::chrono::milliseconds(300));
     std::thread t5(f_pinger);
    // std::thread t6(f_pinger);
     std::this_thread::sleep_for(std::chrono::milliseconds(50));
     std::thread t7(f_pinger);
    // std::thread t8(f_pinger);
    t1.join();
    // t2.join();
    t3.join();
    // t4.join();
    t5.join();
    // t6.join();
     t7.join();
    // t8.join();
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
    int sizeBytes = 0;

    try {
        po::options_description desc("Allowed options");
        desc.add_options()
            ("help", "produce help message")
            ("server,s", po::value(&server_ip)->required(), "server IP")
            ("duration,d", po::value(&duration)->required(), "time of duration in seconds")
            ("client,c", po::bool_switch(&client), "client mode")
            ("quantity,q", po::value(&quantity), "quantity of pings")
            ("sizeBytes,b", po::value(&sizeBytes), "payload in bytes")
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
        return setup_client(server_ip, duration, quantity, sizeBytes);
    else
        return setup_server(server_ip, duration);

    return 0;
}
