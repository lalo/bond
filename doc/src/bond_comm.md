% Bond Communications

**IMPORTANT NOTE: Bond Comm is deprecated. We recommend using
[Bond-over-gRPC](bond_over_grpc.html) for communication. This documentation
is retained for transitional purposes.**

# About #

The Bond Communications framework allows clients and services to exchange
Bond messages.

# Features #

## Defining Services ##

The Bond IDL has been extended to support the definition of
[services](compiler.html#service-definition) and
[generic services](compiler.html#generic-service). These definitions are
used by the Bond compiler to generate:

* service stubs that can be used as the basis for implementing service
  methods
* proxy stubs that can be used by clients to invoke those methods

## Transport Flexibility ##

The Bond service stubs and proxy stubs are not aware of exactly how the
client and server get connected or how messages are exchanged between
clients and servers. These capabilities are encapsulated in a transport.

Bond includes the following transports:

* [Epoxy](bond_comm_epoxy.html): asynchronous communication over TCP
  connections with support for TLS using a binary protocol
* [SimpleInMem](bond_comm_simpleinmem.html): asynchronous communication over in-memory
  connections using a binary protocol

Not all transports support all messaging patterns. Check the documentation
for the respective transports.

## Messaging Patterns ##

There are a number of conceptual messaging patterns that are supported:

* request/response: a message is sent to the receiver and the receiver sends
  back a different message (including the ability to send back an error
  response)
* event: a one-way, best effort, message is sent to a receiver. There's no
  way to know whether the message was delivered or processed successfully.

### Future Messaging Patterns ###

We expect to implement additional messaging patterns in the future. Patterns
that are intriguing include--but are not limited to:

* sequences of requests/responses: similar to request/response, but with a
  sequence of requests/responses of unknown count
* message bus/broadcast patterns: send the same request to a bunch of
  listeners, with a variety of ways to collect responses--if any
* topics/subscriptions: publish a stream of events and receive a filtered
  view of that stream
* aggregations: send requests to multiple receivers and combine their
  responses in various ways

## Layers ##

Bond Comm includes extensibility points called "layers" that provides hooks
for capabilites that are not service- or method-specific. An example of this
would be tracing hooks like those mentioned in the [Dapper
paper](http://research.google.com/pubs/pub36356.html).

A set of layers are encapsulated together in a "layer stack", which can be
provided to the Transport. When the Transport sends out a message, the layer
stack invokes the layers in forward order. On the receive side, the layer
stack invokes the layers in reverse order.

The layers in a layer stack are expected to share a single Bond structure
for passing state from the send side to the receive side. This structure is
serialized by the transport and sent alongside the actual message payload.

If any layer returns an error, the layer stack is halted and the error
replaces the current message; therefore, layers should only return errors
for conditions that are truly fatal for the current message.

Layers may be stateful or stateless. In the case of request/response messages,
a stateful layer instance is shared across the two halves of the conversation
-- i.e. on the client side, a stateful layer instance can pass its state from the
outbound request to the inbound response, and on the server side, from the
inbound request to the outbound response.

# Implementations #

Bond Communications is available for [C#](bond_cs.html#bond-comm) and [C++](bond_cpp.html#bond-comm).
