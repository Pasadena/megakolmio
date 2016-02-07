import KolmioApp.{Figure, MiniKolmio}
import org.scalatest._

/**
  * Created by spokos on 2/7/16.
  */
class KolmioAppTests extends FlatSpec with Matchers {

  it should "throw IllegalArgumentException when square root of kolmio amount is not event number " in {
    val kolmios = List(MiniKolmio("P1", Figure.FOX_TOP, Figure.FOX_BOTTOM, Figure.DEER_TOP),
      MiniKolmio("P2", Figure.DEER_TOP, Figure.FOX_BOTTOM, Figure.RACCOON_BOTTOM))
    a[IllegalArgumentException] should be thrownBy {
      KolmioApp.createMegaKolmio(kolmios)
    }
  }

  "Megakolmio with one element" should "always be in correct order" in {
    val testKolmios = List(MiniKolmio("P1", Figure.FOX_TOP, Figure.FOX_BOTTOM, Figure.DEER_TOP))
    val result = KolmioApp.createMegaKolmio(testKolmios)
    result.flatten.size should be (1)
    result.flatten.head.identifier should be ("P1")
  }

  "Megakolmio with four elements" should "contains two rows with one and three elements" in {
    val testKolmios = List(MiniKolmio("P1", Figure.FOX_TOP, Figure.FOX_BOTTOM, Figure.DEER_TOP),
      MiniKolmio("P9", Figure.FOX_BOTTOM, Figure.DEER_TOP, Figure.DEER_BOTTOM),
      MiniKolmio("P2", Figure.DEER_TOP, Figure.FOX_BOTTOM, Figure.RACCOON_BOTTOM),
      MiniKolmio("P6", Figure.RACCOON_BOTTOM, Figure.FOX_BOTTOM, Figure.RACCOON_TOP))
    val result = KolmioApp.createMegaKolmio(testKolmios)
    result.flatten.size should be (4)
    result(0).size should be (1)
    result(1).size should be (3)
  }
}
