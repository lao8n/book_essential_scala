## Case Classes

*Case classes* are an exceptionally useful shorthand for defining a class, a companion object, and a lot of sensible defaults in one go. They are ideal for creating lightweight data-holding classes with the minimum of hassle.

Case classes are created simply by prepending a class definition with the keyword `case`:

~~~ scala
case class Person(firstName: String, lastName: String) {
  def name = firstName + " " + lastName
}
~~~

Whenever we declare a case class, Scala automatically generates a *class and companion object*:

~~~ scala
val dave = new Person("Dave", "Gurnell") // we have a class
// dave: Person = Person(Dave,Gurnell)

Person // and a companion object too
// res: Person.type = Person
~~~

What's more, the class and companion are pre-populated with some very useful features.

### Features of a case class

1. *A field for each constructor argument*---we don't even need to write `val` in our constructor definition, although there's no harm in doing so.

~~~ scala
dave.firstName
// res: String = Dave
~~~

2. *A default `toString` method* that prints a sensible constructor-like representation of the class (no more `@` signs and cryptic hex numbers):

~~~ scala
dvae
// res: Person = Person("Dave","Gurnell")
~~~

3. *Sensible `equals`, and `hashCode` methods* that operate on the field values in the object.

   This makes it easy to use case classes with collections like `Lists`, `Sets` and `Maps`. It also means we can compare objects on the basis of their contents rather than their reference identity:

~~~ scala
new Person("Noel", "Welsh").equals(new Person("Noel", "Welsh"))
// res: Boolean = true

new Person("Noel", "Welsh") == new Person("Noel", "Welsh")
// res: Boolean = true
~~~


4. *A `copy` method* that creates a new object with the same field values as the current one:

~~~ scala
dave.copy()
// res: Person = Person(Dave,Gurnell)
~~~

   Note that the `copy` method creates and returns a *new object* of the class rather than returning the current one:

~~~ scala
dave.copy() eq res0
// res: Boolean = false
~~~

   The `copy` method actually accepts optional parameters matching each of the constructor parameters. If a parameter is specified the new object uses that value instead of the existing value from the current object. This is ideal for use with keyword parameters to let us copy an object while changing the values of one or more fields:

~~~ scala
dave.copy(firstName = "Dave2")
// res: Person = Person(Dave2,Gurnell)

dave.copy(lastName = "Gurnell2")
// res: Person = Person(Dave,Gurnell2)
~~~

<div class="callout callout-info">

#### Value and Reference Equality {-}

Scala's `==` operator is different from Java's---it delegates to `equals` rather than comparing values on reference identity.

Scala has an operator called `eq` with the same behaviour as Java's `==`. However, it is rarely used in application code:

~~~ scala
new Person("Noel", "Welsh") eq (new Person("Noel", "Welsh"))
// res: Boolean = false

dave eq dave
// res: Boolean = true
~~~
</div>

### Features of a case class companion object

The companion object contains an `apply` method with the same arguments as the class constructor. Scala programmers tend to prefer the `apply` method over the constructor for the brevity of omitting `new`, which makes constructors much easier to read inside expressions:

~~~ scala
Person("Dave", "Gurnell") == Person("Noel", "Welsh")
// res: Boolean = false

Person("Dave", "Gurnell") == Person("Dave", "Gurnell")
// res: Boolean = true
~~~

Finally, the companion object also contains code to implement an *extractor pattern* for use in *pattern matching*. We'll see this later this chapter.

<div class="callout callout-info">
#### Case Class Declaration Syntax {-}

The syntax to declare a case class is

~~~ scala
case class Name(parameter: type, ...) {
  declarationOrExpression ...
}
~~~

where

- `Name` is the name of the case class;
- the optional `parameter`s are the names given to constructor parameters;
- the `type`s are the types of the constructor parameters;
- the optional `declarationOrExpression`s are declarations or expressions.
</div>

### Case objects

A final note. If you find yourself defining a case class with no constructor arguments you can instead a define a *case object*. A case object is defined just like a case class and has the same default methods as a case class.

~~~ scala
case object Citizen {
  def firstName = "John"
  def lastName  = "Doe"
  def name = firstName + " " + lastName
}
~~~

The differences between a case object and a regular singleton object are:

 - The `case object` keyword defines a class and an object, and makes the object an instance (actually the only instance) of the class:

~~~ scala
class Citizen { /* ... */ }
object Citizen extends Citizen { /* ... */ }
~~~

 - With a case object we still get all of the functionality defined for case classes above.

### Take Home Points

Case classes are the *bread and butter of Scala data types*. Use them, learn them, love them.

The syntax for declaring a case class is the same as for declaring a class, but with `case` appended

~~~ scala
case class Name(parameter: type, ...) {
  declarationOrExpression ...
}
~~~


Case classes have numerous auto-generated methods and features that save typing. We can override this behaviour on a piece-by-piece basis by implementing the relevant methods ourselves.

In Scala 2.10 and earlier we can define case classes containing 0 to 22 fields. In Scala 2.11 we gain the ability to define arbitrarily-sized case classes.

### Exercises

#### Case Cats

Recall that a `Cat` has a `String` colour and food. Define a case class to represent a `Cat`.

<div class="solution">
Another simple finger exercise.

~~~ scala
case class Cat(colour: String, food: String)
~~~
</div>

#### Roger Ebert Said it Best...

> No good movie is too long and no bad movie is short enough.

The same can't always be said for code, but in this case we can get rid of a lot of boilerplate by converting `Director` and `Film` to case classes. Do this conversion and work out what code we can cut.

<div class="solution">

Case classes provide our `copy` methods and our `apply` methods and remove the need to write val` before each constructor argument. The final codebase looks like this:

~~~ scala
case class Director(firstName: String, lastName: String, yearOfBirth: Int) {
  def name: String =
    s"$firstName $lastName"
}

object Director {
  def older(director1: Director, director2: Director): Director =
    if (director1.yearOfBirth < director2.yearOfBirth) director1 else director2
}

case class Film(
  name: String,
  yearOfRelease: Int,
  imdbRating: Double,
  director: Director) {

  def directorsAge =
    yearOfRelease - director.yearOfBirth

  def isDirectedBy(director: Director) =
    this.director == director
}

object Film {
  def newer(film1: Film, film2: Film): Film =
    if (film1.yearOfRelease < film2.yearOfRelease) film1 else film2

  def highestRating(film1: Film, film2: Film): Double = {
    val rating1 = film1.imdbRating
    val rating2 = film2.imdbRating
    if (rating1 > rating2) rating1 else rating2
  }

  def oldestDirectorAtTheTime(film1: Film, film2: Film): Director =
    if (film1.directorsAge > film2.directorsAge) film1.director else film2.director
}
~~~

Not only is this code significantly shorter, it also provides us with `equals` methods, `toString` methods, and pattern matching functionality that will set us up for later exercises.
</div>

#### Case Class Counter

Reimplement `Counter` as a case class, using `copy` where appropriate. Additionally initialise `count` to a default value of `0`.

<div class="solution">
~~~ scala
case class Counter(count: Int = 0) {
  def dec = copy(count = count - 1)
  def inc = copy(count = count + 1)
  def adjust(adder: Adder) = copy(count = adder(count))
}
~~~

This is almost a trick exercise---there are very few differences with the previous implementation However, notice the extra functionality we got for free:

~~~ scala
Counter(0) // construct objects without `new`
// res: Counter = Counter(0)

Counter().inc // printout shows the value of `count`
// res: Counter = Counter(1)

Counter().inc.dec == Counter().dec.inc // semantic equality check
// res: Boolean = true
~~~
</div>

#### Application, Application, Application

What happens when we define a companion object for a case class? Let's see.

Take our `Person` class from the previous section and turn it into a case class (hint: the code is above). Make sure you still have the companion object with the alternate `apply` method as well.

<div class="solution">
Here's the code:

~~~ scala
case class Person(firstName: String, lastName: String) {
  def name = firstName + " " + lastName
}

object Person {
  def apply(name: String): Person = {
    val parts = name.split(" ")
    apply(parts(0), parts(1))
  }
}
~~~

Even though we are defining a companion object for `Person`, Scala's case class code generator is still working as expected---it adds the auto-generated companion methods to the object we have defined, which is why we need to place the class and companion in a single compilation unit.

This means we end up with a companion object with an overloaded `apply` method with two possible type signatures:

~~~ scala
def apply(name: String): Person // and ...
def apply(firstName: String, lastName: String): Person
~~~
</div>
