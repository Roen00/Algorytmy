package zelek.rafal.algorytmy

/**
  * Zbiór elementów do sortowania musi stanowić liczby całkowite z przedziału (1, k)
  * k <- górna granica wartości danych wejściowych
  * n <- rozmiar danych wejściowych
  * A[n] <- dane wejściowe
  * B[n] <- posortowane dane wejściowe
  * C[k] <- tablica robocza
  *
  * n <- length[A]
  * for i <- 1 to k do
  *   C[i] <- 0
  *
  * for i <- 1 to n do
  *   C[A[i]] <- C[A[i] + 1
  *
  * for i <- 2 to k do
  *   C[i] <- C[i] + C[i - 1]
  *
  * for i <- n downto 1 do
  *   B[C[A[i]]] <- A[i]
  *   C[A[i]] <- C[A[i]] - 1
  *
  */

object CountSort {

  def sort(aInput: List[Int], k: Int): List[Int] = {
    val A = aInput
    /** n <- length[A] */
    val n = A.length

    /** for i <- 1 to k do
      *   C[i] <- 0 */
    val C = Array.fill(k + 1)(0)

    val B = Array.fill(n)(0)

    /** for i <- 1 to n do
      *   C[A[i]] <- C[A[i] + 1 */
    for {
      i <- 0 until n
    } {
      C.update(A(i), C(A(i)) + 1)
    }

    /** for i <- 2 to k do
      *   C[i] <- C[i] + C[i - 1] */
    for {
      i <- 1 to k
    } {
      C.update(i, C(i) + C(i - 1))
    }

    /** for i <- n downto 1 do
      *   B[C[A[i]]] <- A[i]
      *   C[A[i]] <- C[A[i]] - 1 */
    for {
      i <- (0 until n).reverse
    } {
      B.update(C(A(i)) - 1, A(i))
      C.update(A(i), C(A(i)) - 1)
    }
    B.toList
  }
}

object CountSortRunner extends App {
  val data = List(3, 1, 6, 3, 4, 5, 2)
  val kmax: Int = data.max
  val result = CountSort.sort(data, kmax)
  println("Dane wejściowe: " + data.mkString(", "))
  println("Dane posortowane: " + result.mkString(", "))
}
