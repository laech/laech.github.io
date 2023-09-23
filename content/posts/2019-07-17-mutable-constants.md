---
title: Mutable Constants
date: 2019-07-17
draft: false
slug: 2019/07/17/mutable-constants
---

Mutable constants are essentially constants initialized from mutable
variables.

For example, a constant in code but initialized from an environment
variable, which can be mutated before the constant is initialized to
affect it's value:

```java
  static final Language LANG =
    Language.of(getEnv("LANG"));
```

Another example, this time it isn't a constant but it's intended to be
used like a constant after it's initial value is set by some
annotation processing library:

```java
  @Environment("LANG")
  private static Language LANG;

  static Language lang() {
    return LANG;
  }
```

There are undesired implications when code is written to use such
constants.

Take the following program that produces different results based on
the current `LANG` environment variable:

```java
  static final Language LANG =
    Language.of(getEnv("LANG"));

  static String greet() {
    return LANG.translate("Hello");
  }

  static void main(String... args) {
    println(greet());
  }
```

It's **untestable** as you can't unit test `greet()` with different
languages.  One may argue if `LANG` is made to be non-final, you can
change it's value in unit tests in order to test the `greet()`. This
basically turns it into a global variable, which is bad on it's own
levels, introduces concurrency problems, and you wouldn't be able to
test `greet()` with different languages **in parallel**.

It also puts a **hidden dependency** inside `greet()` that drives it's
behavior, not clear to the caller when looking at it's signature. In
the above example, each time you run the program, `greet()` may
produce a different result even though it's called with the same
parameters (in this example, empty). It makes `greet()` a bit magical,
and that's bad, a method's return value should be driven by the input
parameters, not by magic (There maybe exceptions, but this isn't one
of them).

Mutable constants should really be parameters instead:

```java
  static String greet(Language lang) {
    return lang.translate("Hello");
  }

  static void main(String... args) {
    var env = getEnv("LANG");
    var lang = Language.of(env);
    println(greet(lang));
  }
```

This makes the code testable, testable in parallel, dependency is
clear, non-magical.
