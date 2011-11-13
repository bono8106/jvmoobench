package object oobench {

  type SHA1 = {
    def reset(): Unit
    def update(bytes: Array[Byte]): Unit
    def digest(hashout: Array[Byte]): Unit
  }

  type SHA1Factory = {
    val name: String
    def apply(): SHA1
  }

}