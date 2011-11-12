package oobench

object OOBench extends App {

  val fox = "The quick brown fox jumped over the lazy dog.\n".getBytes

  type SHA = oobench.javafor.valuetypes.SHA1

  def doIt(n: Int) {
    val startTime = System.nanoTime()

    var result = 0
    var i = 1
    while (i <= n) {
      val sha1 = new SHA
      sha1.update(fox)
      result += sha1.digest.length
      i += 1
    }

    val endTime = System.nanoTime
    val time = endTime - startTime
    val avgTime: Double = time / n / 1000d
    printf("Executions: %07d; avg time = %.2f micros%n", n, avgTime)
  }

  if (args.length >= 1 && args(0) == "-pause") {
    readLine("Press any key ...")
  }

  doIt(1000)
  doIt(10000)
  doIt(100000)
  doIt(1000000)

}