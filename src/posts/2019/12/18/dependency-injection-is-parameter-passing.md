# Dependency Injection is Parameter Passing

Dependency injection means that instead of a function reaching out to
get its dependencies, it accepts the dependencies as parameters
instead. A dependency may be an external service, or something that
produces observable side effects, etc.

```java
  // Before
  static void isBedTime() {
    var clock = Clock.getSystemClock();
    var time = clock.now();
    return time.hour() >= 23;
  }

  // After
  static void isBedTime(Clock clock) {
    var time = clock.now();
    return time.hour() >= 23;
  }
```

This make the function dumb (in a good way), and the dependencies
clear to the caller. The parameters will typically be some abstract
interfaces, and the concrete implementations can be swapped out to
something different by different callers, such as be mocked out to
make testing easy and deterministic. It's simple and yet effective in
reducing the complexity of large and complex programs, as dependencies
are being passed along instead of having 'smart functions' that go out
and grab things like a spider web.

One may notice doing things this way will cause some functions to
start to have many parameters. This is a good thing to discovery, it's
helping to uncover that the function maybe doing too much, therefore
some refactoring is needed to make it fall in line with the single
responsibility principle, or the facade pattern is needed to be
applied to encapsulate/simplify interactions with some of the
underlying dependencies.

One may also notice that there are various dependency injection
libraries out there, XML based, annotation based, using runtime
reflection, or compile time generation, some may be technically
interesting, but none are necessary, and just like every other code,
they all have their own complexity and issues. Dependency injection at
it's core is just parameter passing, it's dumb and simple, and that's
the way it should be. Less is more.
