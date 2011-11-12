package oobench.scala.ootypes

object IntObject {
  implicit def wrap(n: Int) = new IntObject(n)
  implicit def unwrap(n: IntObject) = n.n

  implicit def asByte(n: IntObject): Byte = n.n.asInstanceOf[Byte]
}

case class IntObject(val n: Int) {
  def +(o: IntObject) = new IntObject(n + o.n)
  def -(o: IntObject) = new IntObject(n - o.n)
  def *(o: IntObject) = new IntObject(n * o.n)
  def /(o: IntObject) = new IntObject(n / o.n)
  def |(o: IntObject) = new IntObject(n | o.n)
  def ^(o: IntObject) = new IntObject(n ^ o.n)
  def &(o: IntObject) = new IntObject(n & o.n)
  def %(o: IntObject) = new IntObject(n % o.n)

  def <<(x: IntObject) = new IntObject(n << x.n)
  def >>(x: IntObject) = new IntObject(n >> x.n)
  def >>>(x: IntObject) = new IntObject(n >>> x.n)

  def ===(x: IntObject) = n == x.n
  def !==(x: IntObject) = n != x.n
  def <(x: IntObject) = n < x.n
  def <=(x: IntObject) = n <= x.n
}
