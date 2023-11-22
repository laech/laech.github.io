# Don't Pass Control Flags Downward

2023-09-24

This is tip 6.2 from [Thinking Forth](https://thinking-forth.sourceforge.net).

Suppose we have the some code that looks like the following:

```java
class Window {
  void setVisible(boolean show) {
    if (show) {
      // ... show the window
    } else {
      // ... hide the window
    }
  }
}

static void main(String... args) {
  var window = ...
  window.setVisible(true);
  // ... do something, then:
  window.setVisible(false);
}
```

We know exactly when the window should be shown/hidden at compile
time, so the calls to `setVisible(boolean)` are asking the program to
make pointless decisions at run time. It also makes the `setVisible`
function trying to do two jobs, show and hide the window, which
violates the single responsibility principle.

So it would be better to just spell out exactly what our intentions are:

```java
class Window {
  void show() {}
  void hide() {}
}

static void main(String... args) {
  var window = ...
  window.show();
  // ... do something, then:
  window.hide();
}
```
