package object oobench {

  type SHA1 = {
    def update(bytes: Array[Byte]): Unit
    def digest(): Array[Byte]
  }

  type SHA1Factory = {
    val name: String
    def apply(): SHA1
  }

}