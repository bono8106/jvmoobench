package oobench.unittests

object SHA1Test extends App {

  private val hexMap = Array[Char]('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f')
  private def toHexByte(sb: StringBuilder, a: Byte) = sb.append(hexMap((a>>4)&0xF)).append(hexMap(a&0xF))
  def toHex(a: Array[Byte]) = a.foldLeft(new StringBuilder(a.length * 2))(toHexByte _).toString

  class FoxTest(sha1: oobench.SHA1) {
    val fox = "The quick brown fox jumped over the lazy dog.\n".getBytes()

    for (i <- 1 to 3) {
      sha1.update(fox)
      val hashout = new Array[Byte](20)
      sha1.digest(hashout)
      val foxHash = toHex(hashout)
      assert (foxHash == "bae5ed658ab3546aee12f23f36392f35dba1ebdd")
    }
  }

  print("Running tests ... ")
  new FoxTest(new oobench.javafor.valuetypes.SHA1)
  new FoxTest(new oobench.scalafor.valuetypes.SHA1)
  new FoxTest(new oobench.scalafor.objects.SHA1)
  new FoxTest(new oobench.scalawhile.valuetypes.SHA1)
  new FoxTest(new oobench.scalawhile.objects.SHA1)
  new FoxTest(new oobench.scalawhile.objects2.SHA1)
  println("SUCCESS")

}