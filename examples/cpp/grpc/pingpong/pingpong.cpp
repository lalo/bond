
#include "pingpong_grpc.h"
#include "pingpong_types.h"

#pragma warning(push)
#pragma warning(disable: 4505)
#include <boost/program_options.hpp>
#pragma warning(pop)
#include <boost/algorithm/string/predicate.hpp>

#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/server.h>
#include <bond/ext/grpc/server_builder.h>
#include <bond/ext/grpc/thread_pool.h>
#include <bond/ext/grpc/unary_call.h>

#include <string>
#include <istream>
#include <ostream>
#include <sstream>
#include <memory>

#include <ppl.h>    // For concurrency::combinable<T>

namespace po = boost::program_options;


enum class TransportType
{
    Grpc,
};

std::istream& operator>>(std::istream& is, TransportType& type)
{
    static std::locale s_loc;

    std::string str;
    is >> str;

    if (boost::iequals(str, "Grpc", s_loc))
    {
        type = TransportType::Grpc;
    }
    else
    {
        throw po::invalid_option_value{ str };
    }

    return is;
}

std::ostream& operator<<(std::ostream& os, const TransportType& type)
{
    switch (type)
    {
    case TransportType::Grpc:
        return os << "Grpc";
    }

    return os << "Unknown[" << static_cast<int>(type) << "]";
}

class Options
{
public:
    Options(int argc, char* argv[])
    {
        po::options_description desc{ "RpcPerf.exe program options" };
        desc.add_options()
            ("help,h", "Print usage message")
            ("transport,t", po::value<TransportType>(&m_transport)->required(), "Transport type [ Grpc ]")
            ("server", po::bool_switch(&m_server)->required(), "Host server")
            ("client", po::bool_switch(&m_client)->required(), "Run client")
            ("ip", po::value<std::string>(&m_ip)->default_value("127.0.0.1"), "IP Address")
            ("port", po::value<std::uint16_t>(&m_port)->default_value(9143), "Port number")
            ("threads", po::value<std::uint32_t>(&m_threads)->default_value(std::thread::hardware_concurrency()), "Thread count")
            ("qps", po::value<std::uint32_t>(&m_qps)->default_value(10), "Queries per second")
            ("payload", po::value<std::uint32_t>(&m_payload)->default_value(1024), "Payload size in bytes")
            ("seconds", po::value<std::uint32_t>(&m_seconds)->default_value(0), "Duration of test, infinite if 0 until key press")
            ("warmupseconds", po::value<std::uint32_t>(&m_warmup_seconds)->default_value(0), "Duration of warmup")
            ;

        po::variables_map vm;
        po::store(po::parse_command_line(argc, argv, desc), vm);

        if (vm.count("help"))
        {
            std::cout << desc << std::endl;
            throw std::exception{ "Done" };
        }

        po::notify(vm);
    }

    const auto& GetTransport() const { return m_transport; }

    const auto& GetServer() const { return m_server; }

    const auto& GetClient() const { return m_client; }

    const auto& GetIP() const { return m_ip; }

    const auto& GetPort() const { return m_port; }

    const auto& GetThreads() const { return m_threads; }

    const auto& GetQPS() const { return m_qps; }

    const auto& GetPayoad() const { return m_payload; }

    const auto& GetSeconds() const { return m_seconds; }

    const auto& GetWarmupSeconds() const { return m_warmup_seconds; }

private:
    TransportType m_transport;
    bool m_server;
    bool m_client;
    std::string m_ip;
    std::uint16_t m_port;
    std::uint32_t m_threads;
    std::uint32_t m_qps;
    std::uint32_t m_payload;
    std::uint32_t m_seconds;
    std::uint32_t m_warmup_seconds;
};


std::ostream& operator<<(std::ostream& os, const Options& options)
{
    return os   << "Transport=" << options.GetTransport()
                << ", Address=" << options.GetIP() << ":" << options.GetPort()
                << ", Threads=" << options.GetThreads()
                << ", QPS=" << options.GetQPS()
                << ", Payload=" << options.GetPayoad();
}


class PerfServiceImpl final : public Ping::Perf::Service
{
private:
    void Invoke(
        bond::ext::gRPC::unary_call<
            bond::bonded<Ping::Data>,
            Ping::Data> call) override
    {
        try
        {
            Ping::Data request = call.request().Deserialize();

            call.Finish(request);
        }
        catch (const std::exception& e)
        {
            constexpr auto c_failureLogFreq = 1'000UL;

            if (++*m_failed % c_failureLogFreq == 0)
            {
                std::cout << e.what() << std::endl;
            }
        }
    }

    void KillServer(
        bond::ext::gRPC::unary_call<
            bond::bonded< ::bond::Void>,
            bond::Void> call) override
    {
        call.Finish(bond::Void());
    }

private:
    std::shared_ptr<std::atomic_size_t> m_failed{ std::make_shared<std::atomic_size_t>(0) };
};


class NetworkImpl
{
public:
    template <typename... Args>
    explicit NetworkImpl(const std::string& address, Args&&... args)
        : m_address{ address }
    {}

    auto Bind()
    {
        bond::ext::gRPC::server_builder builder;
        builder.SetThreadPool(threadPool);

        builder.AddListeningPort(m_address, grpc::InsecureServerCredentials());
        builder.RegisterService(&perf_service);

        return std::unique_ptr<bond::ext::gRPC::server>(builder.BuildAndStart());
    }

    auto Connect()
    {
        /*
        grpc::ChannelArguments channel_args;
        channel_args.SetInt(GRPC_ARG_HTTP2_WRITE_BUFFER_SIZE, 3000);
        auto channel = grpc::CreateCustomChannel(m_address, grpc::InsecureChannelCredentials(), channel_args);
        */

        auto channel = grpc::CreateChannel(m_address, grpc::InsecureChannelCredentials());

        channel->WaitForConnected(std::chrono::system_clock::now() + std::chrono::seconds(5));

        return std::make_shared<Ping::Perf::Client>(
            channel,
            ioManager,
            threadPool);
    }

private:
    std::string m_address;
    PerfServiceImpl perf_service;
    std::shared_ptr<bond::ext::gRPC::thread_pool> threadPool = std::make_shared<bond::ext::gRPC::thread_pool>();
    std::shared_ptr<bond::ext::gRPC::io_manager> ioManager = std::make_shared<bond::ext::gRPC::io_manager>();
};


template <typename T>
class BasicObjectPool
{
    class Deleter;

public:
    template <typename... Args>
    std::unique_ptr<T, Deleter> Take(Args&&... args)
    {
        std::unique_ptr<T> obj;

        {
            std::lock_guard<std::mutex> guard{ _impl->_lock };

            if (_impl->_pool.empty())
            {
                obj = std::make_unique<T>(std::forward<Args>(args)...);
            }
            else
            {
                obj = std::move(_impl->_pool.back());
                _impl->_pool.pop_back();
            }
        }

        auto ptr = obj.get();
        return std::unique_ptr<T, Deleter>{ ptr, Deleter{ _impl, std::move(obj) } };
    }

    template <typename... Args>
    std::shared_ptr<T> TakeShared(Args&&... args)
    {
        auto obj = Take(std::forward<Args>(args)...);
        auto ptr = obj.get();

        return std::shared_ptr<T>{ std::make_shared<decltype(obj)>(std::move(obj)), ptr };
    }

    std::size_t GetCapacity() const
    {
        std::lock_guard<std::mutex> guard{ _impl->_lock };
        return _impl->_pool.size();
    }

private:
    struct Impl
    {
        std::mutex _lock;
        std::vector<std::unique_ptr<T>> _pool;
    };

    class Deleter
    {
    public:
        Deleter(const std::shared_ptr<Impl>& impl, std::unique_ptr<T> obj)
            : _impl{ impl },
              _obj{ std::move(obj) }
        {}

        template <typename T>
        void operator()(T* obj)
        {
            (void)obj;
            assert(obj == _obj.get());

            if (auto impl = _impl.lock())
            {
                std::lock_guard<std::mutex> guard{ impl->_lock };
                impl->_pool.push_back(std::move(_obj));
            }
            else
            {
                _obj.reset();
            }
        }

    private:
        std::weak_ptr<Impl> _impl;
        std::unique_ptr<T> _obj;
    };

    std::shared_ptr<Impl> _impl = std::make_shared<Impl>();
};

template <typename Impl>
class PerfTest
{
public:
    template <typename... Args>
    PerfTest(const Options& options, Args&&... args)
        : m_options{ options },
          m_impl{ std::forward<Args>(args)... }
    {}

    auto HostServer()
    {
        return m_impl.Bind();
    }

    void RunClient()
    {
        auto buffer = std::make_unique<std::uint8_t[]>(m_options.GetPayoad());
        std::generate(buffer.get(), buffer.get() + m_options.GetPayoad(), [] {return static_cast<std::uint8_t>(std::rand());});

        Ping::Data request;
        request.data = bond::blob{ buffer.get(), static_cast<std::uint32_t>(m_options.GetPayoad()) };

        using Latencies = std::vector<std::chrono::microseconds>;
        using CombinableLatencies = concurrency::combinable<Latencies>;

        auto latencies = std::make_shared<CombinableLatencies>();
        auto failed = std::make_shared<std::atomic_size_t>(0);
        auto ok = std::make_shared<std::atomic_size_t>(0);
        std::mutex maxPendingLock;
        std::condition_variable cv_pending;
        auto pending = std::make_shared<std::atomic_size_t>(0);
        std::atomic_bool stop{ false };
        std::atomic_bool warm_up{ true };

        static const auto c_failureLogFreq = m_options.GetQPS() * 5;

        static constexpr auto c_msPerSecond = 1'000UL;

        std::size_t batch;
        std::chrono::milliseconds duration;

        if (m_options.GetQPS() < c_msPerSecond)
        {
            batch = 1;
            duration = std::chrono::milliseconds{ c_msPerSecond / m_options.GetQPS() };
        }
        else
        {
            duration = std::chrono::milliseconds{ 1 };
            batch = m_options.GetQPS() / (c_msPerSecond * duration.count());
        }

        auto failure = [failed](const char* msg = "Dropped", std::size_t count = 1)
        {
            auto f = failed->fetch_add(count);

            if (((f + count) / c_failureLogFreq) > (f / c_failureLogFreq))
            {
                std::cout << msg << std::endl;
            }
        };

        auto makeCallback = [&request, ok, failure, latencies = std::weak_ptr<CombinableLatencies>{ latencies }]
        {
            auto measure = [ok, latencies, begin = std::chrono::high_resolution_clock::now()]()
            {
                auto end = std::chrono::high_resolution_clock::now();

                if (auto l = latencies.lock())
                {
                    ++*ok;
                    l->local().push_back(std::chrono::duration_cast<std::chrono::microseconds>(end - begin));
                }
            };

            return [&request, ok, failure, measure = std::move(measure)](std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::Ping::Data>> call)
            {
                if(call->status.ok())
                {
                    auto response = call->response.Deserialize();
                    //assert(request == response);
                    measure();
                }
                else
                {
                    failure(call->status.error_message().c_str());
                }
            };
        };

        struct Barrier
        {
        public:
            explicit Barrier(std::size_t count)
                : m_count{ count }
            {}

            void Wait()
            {
                std::unique_lock<std::mutex> guard{ m_lock };

                if (m_count != 0)
                {
                    if (--m_count == 0)
                    {
                        m_cv.notify_all();
                    }
                    else
                    {
                        m_cv.wait(guard, [this] { return m_count == 0; });
                    }
                }
            }

        private:
            std::mutex m_lock;
            std::condition_variable m_cv;
            std::size_t m_count;
        };

        Barrier barrier{ m_options.GetThreads() + 1 };

        Barrier w_barrier{ m_options.GetThreads() + 1 };

        std::vector<std::thread> threads{ m_options.GetThreads() };

        auto drain = [&]
        {
            std::unique_lock<std::mutex> guard{ maxPendingLock };
            cv_pending.wait(guard, [&] { return *pending == 0; });
        };

        for (auto& t : threads)
            t = std::thread([&, proxy = m_impl.Connect(), maxPending = std::size_t(0)] () mutable
            {
                barrier.Wait();

                while (warm_up)
                {
                    auto beginBatch = std::chrono::system_clock::now();

                    for (std::size_t i = 0; (i < batch) && warm_up && !stop; ++i)
                    {
                        ++*pending;
                        proxy->AsyncInvoke(/*std::move(context),*/ request, [proxy, &cv_pending, pending](const auto&) {
                            if (--*pending == 0)
                                cv_pending.notify_all();
                        });
                    }

                    std::this_thread::sleep_until(beginBatch + duration);
                }

                drain();
                w_barrier.Wait();

                while (!stop)
                {
                    // Note! Using system_clock instead of high_resolution_clock due to a bug in sleep_until in VC14.
                    auto beginBatch = std::chrono::system_clock::now();

                    for (std::size_t i = 0; (i < batch) && !stop; ++i)
                        try
                        {
                            proxy->AsyncInvoke(/*std::move(context),*/ request,
                                [proxy, callback = makeCallback()](const auto& call)
                            {
                                callback(call);
                            });
                        }
                        catch (const std::exception& e)
                        {
                            failure(e.what());
                        }

                    std::this_thread::sleep_until(beginBatch + duration);
                }
            });

        barrier.Wait();

        if (m_options.GetWarmupSeconds())
        {
            std::cout << "warming up" << std::endl;
            std::this_thread::sleep_for(std::chrono::seconds(m_options.GetWarmupSeconds()));
        }

        warm_up = false;

        w_barrier.Wait();

        drain();

        Wait();

        stop = true;

        for (auto& t : threads)
        {
            t.join();
        }

        Latencies l;

        {
            auto latenciesCopy = *latencies;
            latencies.reset();
            latenciesCopy.combine_each([&l](auto& v) { l.insert(l.end(), v.begin(), v.end()); });

            std::sort(l.begin(), l.end());
        }

        std::cout   << "OK: " << *ok << ", Failed: " << *failed << std::endl
                    << l[std::size_t(0.50 * l.size())].count() << " microseconds, "
                    << l[std::size_t(0.90 * l.size())].count() << " microseconds, "
                    << l[std::size_t(0.95 * l.size())].count() << " microseconds, "
                    << l[std::size_t(0.99 * l.size())].count() << " microseconds, "
                    << std::endl << std::endl;
    }

    void Wait()
    {
        if (m_options.GetSeconds())
        {
            std::this_thread::sleep_for(std::chrono::seconds(m_options.GetSeconds()));
        }
        else
        {
            std::cout << "Press ENTER to exit";
            std::cin.get();
        }
    }

private:
    Options m_options;
    Impl m_impl;
};


template <typename... Args>
void Run(const Options& options, Args&&... args)
{
    PerfTest<NetworkImpl> test{ options, std::forward<Args>(args)... };

    std::unique_ptr<bond::ext::gRPC::server> server;

    if (options.GetServer())
    {
        std::cout << "Hosting server." << std::endl;
        server = test.HostServer();
    }

    if (options.GetClient())
    {
        //std::cout << "Running client." << std::endl;
        test.RunClient();
    }
    else
    {
        test.Wait();
    }
}


int main(int argc, char* argv[])
{
    try
    {
        Options options{ argc, argv };

        std::cout << options << std::endl;

        switch (options.GetTransport())
        {
        case TransportType::Grpc:
            {
                std::ostringstream sstream;
                sstream << options.GetIP() << ":" << options.GetPort();
                std::string full_ip = sstream.str();

                Run(options, full_ip);
            }
            break;
        }
    }
    catch (const std::exception& e)
    {
        std::cout << e.what() << std::endl;
    }

    return 0;
}

