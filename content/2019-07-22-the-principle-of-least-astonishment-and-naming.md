---
title: The Principle of Least Astonishment, and Naming
date: 2019-07-22
draft: false
slug: the-principle-of-least-astonishment-and-naming
---

The Principle of Least Astonishment states that code should behave in
expected ways without hidden surprises. The following is an example of
how improperly named things will introduce bugs into systems.

Given a list of players, each player has multiple games, we want to
build a map from game IDs to game names. We could do so using a `Map`
and loops Java:

```java
  Map<Integer, String> games = ...;
  for (var player : players) {
    for (var game : player.games()) {
      games.put(
        game.id(),
        game.name()
      );
    }
  }
```

We could also do it using the stream API with the `toMap` collector:

```java
  Map<Integer, String> games =
    players
      .stream()
      .flatMap(Player::games)
      .collect(toMap(
        Game::id,
        Game::name
      ));
```

Both seems reasonable and clear at first.

A `Map` in Java doesn't contain duplicates, when putting a key value
into a map it will override existing mapping that has the same key,
this is standard behavior and everybody expects it. So when multiple
players share the same game, putting the same game ID to name mapping
into a map has no effect.

However the `toMap` collector breaks this expectation, it will throw
an exception if it sees duplicates, this is rather surprising given
the name `toMap` implies it would behave like a normal map would. As
more and more developers are refactoring loops into streams, hopefully
this difference is caught during testing instead of things breaking in
production.

The other problem is you tend to forget about things after a while, so
you started to make the same mistake again by using `toMap` thinking
it behaves like a normal map, only to find youself relearning the
same lesson again, and again, when it surprises you by breaking
things. Same goes for your friends.

It should have been given an abnormal name that fits with the abnormal
behavior.
