public class FunObj {
  public static void main (String[] args) {
    EStack<Integer> nil = new EStack<Integer>();
    Stack<Integer> s = nil.push(5).push(2).push(7);
    s.print(); // 7 2 5
    Stack<Integer> t = nil.push(4).push(1);
    s.cat(t).print(); // 7 2 5 1 4
    Pair<Stack<Integer>,Stack<Integer>> u = s.cat(t).split(2);
    u.fst().print(); // 7 2
    u.snd().print(); // 5 1 4
    s.isort().print(); // 2 5 7
    t.isort().print(); // 1 4

    s.isort().mrg(t.isort()).print(); // 1 2 4 5 7
    s.tms().print(); // 2 5 7
    s.cat(t).tms().print(); // 1 2 4 5 7

    BST<Integer> ext = new Ext<Integer>();
    BST<Integer> bst = ext.add(5).add(1).add(7).add(0);

    bst.print(); // 0 1 5 7
    System.out.println();
    bst.inorder().print(); // 0 1 5 7
  }
}
