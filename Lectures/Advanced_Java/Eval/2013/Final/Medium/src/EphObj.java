public class EphObj {
  public static void main (String[] args) {
    Stack<Integer> s = new Stack<Integer>();
    s.rm_fst(4);
    s.print(); // empty
    System.out.println("--");
    s.push(5); s.push(1); s.push(3); s.push(1); 
    s.push(0); s.push(3);
    s.print(); // 3 0 1 3 1 5
    s.rm_fst(3);
    s.print(); // 0 1 3 1 5
    s.rm_fst(1);
    s.print(); // 0 3 1 5
    s.rm_fst(5);
    s.print(); // 0 3 1
    s.rm_fst(6);
    s.print(); // 0 3 1

    s.push(1); s.push(5); s.push(3);
    s.print(); // 3 5 1 0 3 1
    s.rm_lst(3);
    s.print(); // 3 5 1 0 1
    s.rm_lst(1);
    s.print(); // 3 5 1 0
    s.rm_lst(3);
    s.print(); // 5 1 0
    s.rm_lst(4);
    s.print(); // 5 1 0

    Stack<Integer> t = new Stack<Integer>();
    t.rm_lst(4);
    t.print(); // empty
    System.out.println("--");
  }
}
