package oobench.unittests

object SHA1Test extends App {

  private val hexMap = Array[Char]('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f')
  private def toHexByte(sb: StringBuilder, a: Byte) = sb.append(hexMap((a>>4)&0xF)).append(hexMap(a&0xF))
  def toHex(a: Array[Byte]) = a.foldLeft(new StringBuilder(a.length * 2))(toHexByte _).toString

  type SHA1Type = {
    def update(bytes: Array[Byte]): Unit
    def digest(): Array[Byte]
  }
  class FoxTest(sha1: SHA1Type) {
    val fox = "The quick brown fox jumped over the lazy dog.\n".getBytes()

    sha1.update(fox)
    val foxHash = toHex(sha1.digest)
    val x = ""
    assert (foxHash == "bae5ed658ab3546aee12f23f36392f35dba1ebdd")
  }

  new FoxTest(new oobench.javafor.valuetypes.SHA1)
  new FoxTest(new oobench.scalafor.valuetypes.SHA1)
  new FoxTest(new oobench.scalafor.objects.SHA1)
  new FoxTest(new oobench.scalawhile.valuetypes.SHA1)
  new FoxTest(new oobench.scalawhile.objects.SHA1)
  new FoxTest(new oobench.scalawhile.objects2.SHA1)

}