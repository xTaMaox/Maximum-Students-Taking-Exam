import scala.annotation.tailrec

object Solution {

  /* Represents a single row pattern with its max possible capacity*/
  private final case class Choice(pat: Int, cap: Int)

  @inline private def unsetLSB(n: Int) = n & (n-1)
  
  /* Hamming weight or number of 1's in binary rep of n */
  @inline private def hw(n: Int) = 
    Iterator.iterate(n)(unsetLSB).takeWhile(_ != 0).size
  
  /* True if r allows cheating in the same row, false otherwise */
  @inline private def ok(r: Int) = 
    ((r & (r << 1)) | (r & (r >> 1))) == 0

  /* Given a row n, returns all possible and permissible 
     permutations of students sitting in this row. 
     Generates the absolute minimum number of permutations 
     per row, therefore optimising space and time on average */
  @tailrec private def perms(n: Int, acc: List[Int] = List(0)): List[Int] = 
    if (n==0) acc 
    else {
      val lsb = n & -n
      perms(unsetLSB(n), acc ++ acc.iterator.map(_ | lsb).filter(ok).toList)
    }
  
  /* True if the given choice c does not allow any cheating in 
     the row r that is right behind c */  
  @inline private def okWith(r: Int)(c: Choice) = 
    ((c.pat & (r << 1)) | (c.pat & (r >> 1))) == 0

  /* Heart of the solution:
   * choices is all possible choices for the row right in front of
   * us at any given moment.
   * pat is a candidate pattern for the current row
   * Returns the best possible choice by iterating over all choices and
   * adding the capacity of pat with the best compatible choice from the
   * choices row (row in front of us). */
  private def gen(choices: List[Choice])(pat: Int) =
    Choice(pat, hw(pat) + choices.iterator.filter(okWith(pat)).map(_.cap).max)
  
  /* Dynamic programming solver. Just lego-ing everything together 
     Initial state is Choice(0, 0) which can be considered as an empty row 
     in the very front of the class. */
  private def maxPossible(rows: Iterator[Int]) =
    rows
      .map(perms(_))
      .foldLeft(List(Choice(0, 0))){ (state, perms) => perms.map(gen(state)) }
      .iterator
      .map(_.cap)
      .max
    
  /* char => bit: 1 = available, 0 = broken */
  @inline private def toBit(seat: Char) = 
    if (seat == '.') 1 else 0

  /* row => binary: 1's denote available seats */
  @inline private def toBinary(bits: Array[Char]) = 
    bits.iterator.map(toBit).reduce { (acc, b) => (acc << 1) + b }

  /* main function */
  def maxStudents(seats: Array[Array[Char]]) =
    maxPossible(seats.iterator.map(toBinary))  

}