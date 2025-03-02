public class EphObj {
  public static void main (String[] args) {
    Queue<Integer> s = new Queue<Integer>();
    s.enqueue(5); s.enqueue(3); s.enqueue(1); s.enqueue(0);
    s.print(); // 5 3 1 0
    Queue<Integer> t = new Queue<Integer>();
    t.enqueue(4); t.enqueue(2);
    t.print(); // 4 2
    s.cat(t);
    s.print(); // 5 3 1 0 4 2
    //   s.rev();
    //    s.print(); // 2 4 0 1 3 5
  }
}
