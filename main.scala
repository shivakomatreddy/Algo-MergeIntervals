case class Interval(start: Int, end: Int)

val intervals = List(Interval(1, 3), Interval(7, 10), Interval(3, 5), Interval(9, 14), Interval(14, 18), Interval(19, 23))

def mergeIntervals(intervals: Seq[Interval]): Seq[Interval] = {
  val sortedIntervals = intervals.sortBy(_.start)
  val stack = new scala.collection.mutable.Stack[Interval]()
  
  stack.push(sortedIntervals.head)
  
  for(interval <- sortedIntervals.tail) {
    
    val lastInterval = stack.pop()
    
    if(lastInterval.end >= interval.start) {
      stack.push(Interval(lastInterval.start, interval.end))  
    } 
    else {
      stack.push(lastInterval)
      stack.push(interval)
    }
    
  }
  stack.toSeq.sortBy(_.start)
}


mergeIntervals(intervals)
