---
title: Pipeline from Input, or Pipeline to Output?
date: 2019-07-26
draft: false
slug: pipeline-from-input-or-pipeline-to-output
---

Let say you are going to implement an HTTP service, we can post it a
list of actions, it will perform those actions and return a list of
results.

The first version is simple, read the actions, process them, then
write the results.

```java
  List<Action> actions =
    readAll(request);

  List<Result> results =
    invokeAll(actions);

  results.forEach(this::write);
```

Everything seems to work fine but after a while clients are starting
to get timeouts.

It turns out that some actions take longer time to complete, say about
1 second, so if you send 10 of these actions to the service, it will
take 10 seconds for the client to see the response after sending the
request, because the service processes everything before sending the
response. Imagine a client sends hundreds or thousands of actions in a
request, it will take a very long time to see the first byte of the
response. And because the client has set its read timeout to be less
than that, it timed out.

Apart from increasing the client timeout, you decided to improve the
service, turning it into a pipeline (`Stream` is lazy sequence):

```java
  Stream<Action> actions =
    read(request);

  Stream<Result> results =
    actions.map(this::invoke);

  results.forEach(this::write);
```

It will now read, invoke, output each of the actions one at a time as
a pipeline. Clients will see the results incrementally as they
complete, no need to wait for everything to be processed first. And this
keeps data flowing continuously, avoiding idle timeouts.

But then you remembered this is no good as it could cause [TCP
Deadlocks](../tcp-deadlock).

Now what other options are there? If a full pipeline is no good, maybe
a partial pipeline would be okay?

For the next option, the service can pipeline from the input, but
collect all the results into a list in memory, once complete, write
them out.

```java
  Stream<Action> actions =
    read(request);

  List<Result> results =
    actions
      .map(this::invoke)
      .collect(toList());

  results.forEach(this::write);
```

This means while the client sends the list of actions, the service
reads and processes the actions one at a time, when the service can't
read fast enough because it encounters slow actions, TCP applies back
pressure to the client, slowing down the rate which it sends data to
the service. The client should see the response soon after it finishes
sending a large request, as processing was done at the same time while
the data was being sent to the service. And since it drains the input
before output, it does not deadlock.

While this is a perfectly fine solution in many cases, in this
specific case there is still a problem. Because TCP has buffers, when
the client finishes sending and switches to waiting for the response,
there maybe enough slow actions left in the buffers (especially with
compression turned on) yet to be received and read by the service,
enough to cause a read timeout.

How about another option, if instead all the actions are read into a
list, then pipeline from processing to output?

```java
  List<Action> actions =
    readAll(request);

  Stream<Result> results =
    actions
      .stream()
      .map(this::invoke);

  results.forEach(this::write);
```

With this, the service would finish loading all actions into memory
soon after the client finishes sending them, as request parsing is
typically fast. Then the service starts processing and writing the
results back one at a time, the client will see each result
incrementally in the response stream. This way we avoids the request
buffering issues with the previous approach.

Does this solve the problem then? Well, it depends. This approach,
like the deadlock prone implementation, requires the client to read
the response output for the actual processing to happen (except for
the first few, if their outputs fit into the TCP buffers). This may or
may not be acceptable depending on the requirements. And what should
the HTTP response code be? If it relies on knowing the outcome of the
actions, then this approach is a no go, since the response code is the
first thing to be sent, it would be fine though if the response code
is generic, and each result has their own result codes instead, again,
depends on the requirements.

So it looks like there is not a satisfactory solution so far. There
are other options though, but they will probably have their own set of
trade offs. Sometimes there is just no perfect solution, but one can
have options, and can pick one that has the best fit.
