package rosalind.assignments.stepic

import scala.collection.mutable.ListBuffer


/**
 * Created by nikita on 11.01.15.
 */
object ros2f {
  def main(args: Array[String]) {
    class LeaderBoardRecord(val peptide:Seq[Int], val score:Int){
      override def toString = score.toString
    }

    def trimLeaderBoard(leaderBoard:List[LeaderBoardRecord], n:Int): List[LeaderBoardRecord] = {
      var positionsCount = n
      var headScore = leaderBoard.head.score
      leaderBoard sortBy (-_.score) takeWhile ( x => {
        if(x.score == headScore){
          true
        }else{
          positionsCount -= 1
          headScore = x.score
          positionsCount > 0
        }
      })
    }

    def trimLeaderBoardRecursive(leaderBoard:List[LeaderBoardRecord], n:Int): List[LeaderBoardRecord] = {
      def iter(leaderBoard:List[LeaderBoardRecord], n:Int, score:Int):List[LeaderBoardRecord] = {
        leaderBoard match {
          case Nil => Nil
          case x::xs => {
            if(x.score != score && n == 1)
              Nil
            else
              x::iter(xs, if(x.score != score) n-1 else n, x.score)
          }
        }
      }
      val leaderBoardSorted = leaderBoard sortBy (-_.score)
      iter(leaderBoardSorted, n, leaderBoardSorted.head.score)
    }

    val leaderBoard = List[LeaderBoardRecord](
       new LeaderBoardRecord(List(1), 10),
       new LeaderBoardRecord(List(2), 9),
       new LeaderBoardRecord(List(3), 9),
       new LeaderBoardRecord(List(4), 8),
       new LeaderBoardRecord(List(5), 7),
       new LeaderBoardRecord(List(6), 7),
       new LeaderBoardRecord(List(6), 7),
       new LeaderBoardRecord(Nil, 6)
    )
    println(trimLeaderBoard(leaderBoard, 4))
    println(trimLeaderBoardRecursive(leaderBoard, 3))
  }
}
