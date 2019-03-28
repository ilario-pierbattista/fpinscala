package fpinscala.errorhandling

case class Person(name: Name, age: Age)

sealed class Name(val value: String)

sealed class Age(val value: Int)

object Name {
  def make(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))
}

object Age {
  def make(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))
}

object Person {
  def make(name: String, age: Int): Either[String, Person] =
    Name.make(name)
      .map2(Age.make(age))(
        Person(_, _)
      )
}
