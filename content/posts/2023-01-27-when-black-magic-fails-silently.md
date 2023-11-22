# When Black Magic Fails Silently

2023-01-27

Sometimes you run into black magic in a code base, the sort of thing
that works but you don't really know why, but you can accept it and
move on, because it all just work.

Except when it shoots you in the foot to remind you it doesn't always
work as advertised. If you are lucky, you'll get a big error screaming
in your face, but often you are not that luckily, it breaks things
silently and carries on.

Take Spring for example, a popular framework in the Java world. Say
you want to run some code within a database transaction, you often
just annotate the method with `@Transactional`:

```java
@Service
class UserService {
  @Transactional
  void updateDetails() {
    // ...
  }
}
```

When `updateDetails()` is call by some other class, it will run
magically inside a transaction. But if it's calls from some method
within the same class, it will silently run the method without a
transaction (this is the default behaviour unless you change it),
resulting in bad data integrity - one of the worst things you would
want to have in a production database, especially if your system deals
with financial transactions.

Another example is that, you may want to perform some common
validations for your service parameters:

```java
@Service
class UserService {
  void updateName(@NotEmpty String name) {
    // ...
  }
}
```

This won't work because you forgot to annotate the class with
`@Validated` (which is easily missed if your class already has half a
dozen annotations), so again it will silently do nothing and the
method will happily accept invalid input, or worse, causing security
issues.

Yet another example, you want to listen to certain events and want to
perform some action after the associated database transaction commits
successfully:

```java
class UserEventListener {
  @TransactionalEventListener
  void onEvent(UserUpdated event) {
    // ...
  }
}
```

This works until you want to write to the database within the event
method. Your writes will appear to return successfully, but in fact
they are all dropped silently behind the scenes, and this is a
documented expected behaviour. Shocking.
