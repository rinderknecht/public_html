public class SFunObj {
  public static void main (String[] args) {
    Stack<Integer> s = (new EStack<Integer>()).push(7).push(3).push(1).push(3).push(5);
    s.print(); // 5 3 1 3 7
    s.isort().print(); // 1 3 3 5 7
    Stack<Integer> a = (new EStack<Integer>()).push(54).push(46).push(31).push(1);
    Stack<Integer> b = (new EStack<Integer>()).push(66).push(50).push(39);
    a.merge(b).print(); // 1 31 39 46 50 54 66
    a.to_BST().inorder(); // 1 31 46 54
  }
}
