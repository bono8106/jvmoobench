package oobench

object OOBench extends App {

  def doIt(factory: SHA1Factory, n: Int) {
    val fox = "The quick brown fox jumped over the lazy dog.\n".getBytes
    val startTime = System.nanoTime()

    var result = 0
    var i = 1
    while (i <= n) {
      val sha1 = factory()
      sha1.update(fox)
      result += sha1.digest.length
      i += 1
    }

    val endTime = System.nanoTime
    val time = endTime - startTime
    val avgTime: Double = time / n / 1000d
    printf("  executions: %07d; avg time = %.2f micros%n", n, avgTime, result)
  }

  if (args.length >= 1 && args(0) == "-pause") {
    readLine("Press Enter to begin ...")
  }

  val factories = List[SHA1Factory](
      javafor.valuetypes.SHA1Factory,
      scalawhile.valuetypes.SHA1,
      scalawhile.objects.SHA1,
      scalawhile.objects2.SHA1,
      scalafor.valuetypes.SHA1,
      scalafor.objects.SHA1)

  for (fac <- factories) {
    println("Testing " + fac.name)
    doIt(fac, 1000)
    doIt(fac, 10000)
    doIt(fac, 100000)
    doIt(fac, 1000000)
  }

}
