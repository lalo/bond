
#ifdef _MSC_VER
    #pragma warning(disable : 4505) // disable "unreferenced local function has been removed" warning
#endif

#include <bond/ext/grpc/thread_pool.h>

// TODO: move unit_test_framework.h to cpp/test/inc
#include "../core/unit_test_framework.h"

#include <boost/chrono.hpp>
#include <functional>
#include <mutex>

class BasicThreadPoolTests
{
    static
    void addOne(int* i)
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
        (*i)++;
    }

    static
    void UseStdFunction()
    {
        bond::ext::thread_pool threads(1);
        int sum = 0;

        std::function<void(int*)> f_addOne = addOne;

        threads.schedule(std::bind(f_addOne, &sum));

        std::this_thread::sleep_for(std::chrono::milliseconds(300));

        UT_AssertIsTrue(sum == 1);
    }

    static
    void FinishAllTasksAfterDelete()
    {
        bond::ext::thread_pool* threads = new bond::ext::thread_pool(2);
        int sum = 0;
        std::mutex sum_mutex;

        threads->schedule([&sum, &sum_mutex](){
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
            std::lock_guard<std::mutex> lock(sum_mutex);
            sum++;
        });

        threads->schedule([&sum, &sum_mutex](){
            std::this_thread::sleep_for(std::chrono::milliseconds(200));
            std::lock_guard<std::mutex> lock(sum_mutex);
            sum++;
        });

        threads->schedule([&sum, &sum_mutex](){
            std::this_thread::sleep_for(std::chrono::milliseconds(300));
            std::lock_guard<std::mutex> lock(sum_mutex);
            sum++;
        });

        threads->schedule([&sum, &sum_mutex](){
            std::this_thread::sleep_for(std::chrono::milliseconds(400));
            std::lock_guard<std::mutex> lock(sum_mutex);
            sum++;
        });

        // blocks until all schedule tasks are finished
        delete threads;

        UT_AssertIsTrue(sum == 4);
    }

public:

    static
    void Initialize()
    {
        std::string name = "ThreadPool";
        UnitTestSuite suite(name.c_str());

        suite.AddTestCase(UseStdFunction, "UseStdFunction");
        suite.AddTestCase(FinishAllTasksAfterDelete, "FinishAllTasksAfterDelete");
    }
};

bool init_unit_test()
{
    BasicThreadPoolTests::Initialize();
    return true;
}
