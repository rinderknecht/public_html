public class EphObj {
  public static void main (String[] args) {
    Stack<Integer> s = new Stack<Integer>();
    s.push(5); s.push(3); s.push(1); s.push(0);
    s.print(); // 0 1 3 5
    Stack<Integer> t = new Stack<Integer>();
    t.push(4); t.push(2);
    t.print(); // 2 4
    t.rev_cat(s);
    t.print(); // 5 3 1 0 2 4
    t.rev();
    t.print(); // 4 2 0 1 3 5
    //    t.cat(s);
    //    t.print(); // 2 4 0 1 3 5
    //    t.msort(); // 0 1 2 3 4 5
    // t.isort();
    // t.merge_opt(s);
    // t.print(); // 0 1 2 3 4 5
    // s.print();
    // Cell<Integer> u = t.split();
    // t.print(); // 0 1 2
    // u.print(); // 3 4 5
  }
}
