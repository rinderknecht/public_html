public class FunObj {
  public static void main (String[] args) {
    final Queue<Integer> empty = new EQueue<Integer>();
    final Queue<Integer> q =
      empty.enqueue(5).enqueue(7).enqueue(0).enqueue(1); 
    q.print(); // 5 7 0 1
    System.out.println();
    try {
      Pair<Integer,Queue<Integer>> a = q.dequeue();
      a.snd().print(); // 7 0 1
    }
    catch (QEmpty x) { }
    System.out.println();
  }
}
