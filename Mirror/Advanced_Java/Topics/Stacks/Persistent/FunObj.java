public class FunObj {
  public static void main (String[] args) {
    final Stack<Integer> empty = new EStack<Integer>();
    final Stack<Integer> s = empty.push(5).push(7).push(0).push(1); 
    s.print(); // 1 0 7 5
    s.rev().print(); // 5 7 0 1
    final Stack<Integer> t = empty.push(6).push(2);
    t.print(); // 2 6
    final Stack<Integer> u = s.cat(t);
    u.print(); // 1 0 7 5 2 6
    u.isort().print(); // 0 1 2 5 6 7
  }
}
