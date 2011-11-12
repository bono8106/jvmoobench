package oobench.scala.ootypes

object LongObject {
  implicit def wrap(n: Long) = new LongObject(n)
  implicit def unwrap(n: LongObject) = n.n

  implicit def asByte(n: LongObject): Byte = n.n.asInstanceOf[Byte]
}

case class LongObject(val n: Long) {
  def +(o: LongObject) = new LongObject(n + o.n)
  def -(o: LongObject) = new LongObject(n - o.n)
  def *(o: LongObject) = new LongObject(n * o.n)
  def /(o: LongObject) = new LongObject(n / o.n)
  def |(o: LongObject) = new LongObject(n | o.n)
  def ^(o: LongObject) = new LongObject(n ^ o.n)
  def &(o: LongObject) = new LongObject(n & o.n)
  def %(o: LongObject) = new LongObject(n % o.n)

  def <<(x: LongObject) = new LongObject(n << x.n)
  def >>(x: LongObject) = new LongObject(n >> x.n)
  def >>>(x: LongObject) = new LongObject(n >>> x.n)
}
