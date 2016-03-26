package oobench.javafor.valuetypes

object SHA1Factory extends oobench.SHA1Factory {

  val name = "Java (baseline performance target)"

  def apply(): oobench.SHA1 = new SHA1()

}