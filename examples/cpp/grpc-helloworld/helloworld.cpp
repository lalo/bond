#include "helloworld_types.h"
#include "helloworld_grpc.h"
#include <bond/ext/grpc/thread_pool.h>

#pragma warning (push)
#pragma warning (disable: 4100)

#include <grpc++/grpc++.h>

using grpc::Channel;
using grpc::ClientContext;
using grpc::Status;

using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;

using namespace helloworld;

class GreeterClient {
 public:
  GreeterClient(std::shared_ptr<Channel> channel)
      : stub_(Greeter::NewStub(channel)) {}

  // Assembles the client's payload, sends it and presents the response back
  // from the server.
  std::string SayHello(const std::string& user) {
    ClientContext context;

    HelloRequest request;
    request.name = "world";

    HelloReply reply;

    // The actual RPC.
    bond::comm::message<HelloRequest> req(request);
    bond::comm::message<HelloReply> rep;
    Status status = stub_->SayHello(&context, req, &rep);

    if (status.ok()) {
      return rep.value().Deserialize().message;
    } else {
      std::cout << status.error_code() << ": " << status.error_message()
                << std::endl;
      return "RPC failed";
    }
  }

 private:
  std::unique_ptr<Greeter::Stub> stub_;
};

// Logic and data behind the server's behavior.
class GreeterServiceImpl final : public Greeter::Service {
  Status SayHello(ServerContext* context, const bond::comm::message<HelloRequest>* request,
                  bond::comm::message<HelloReply>* reply) override {
    HelloReply real_reply;
    real_reply.message = "hello " + request->value().Deserialize().name;

    bond::comm::message<HelloReply> rep(real_reply);
    *reply = rep;

    return Status::OK;
  }
};

int main()
{
    bond::ext::thread_pool threads(2);

    std::string server_address("0.0.0.0:50051");
    GreeterServiceImpl service;

    ServerBuilder builder;
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
    builder.RegisterService(&service);
    std::unique_ptr<Server> server(builder.BuildAndStart());

    Server* server_ptr = server.get();

    threads.schedule([&server_ptr](){
        server_ptr->Wait();
    });

    std::string reply;

    threads.schedule([&reply](){
        //wait for server to be alive
        std::this_thread::sleep_for(std::chrono::milliseconds(200));

        GreeterClient greeter(grpc::CreateChannel("localhost:50051", grpc::InsecureChannelCredentials()));
        std::string user("world");
        reply = greeter.SayHello(user);
    });

    std::this_thread::sleep_for(std::chrono::milliseconds(300));

    threads.schedule([&server_ptr](){
        auto deadline = std::chrono::system_clock::now() + std::chrono::microseconds(1000);
        server_ptr->Shutdown(deadline);
    });

    if (strcmp(reply.c_str(), "hello world") == 0)
    {
        return 0;
    }
    else
    {
        std::cout << "Did not receive correct response. received: " << reply;
        return 1;
    }
}

#pragma warning (pop)
