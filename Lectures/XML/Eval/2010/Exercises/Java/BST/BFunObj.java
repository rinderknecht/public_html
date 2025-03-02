public class BFunObj {
  public static void main (String[] args) throws EmptyBST {
    BST<Integer> t = (new Ext<Integer>()).add(5).add(1).add(7).add(0);

    t.inorder(); // 0 1 5 7
    System.out.println();
    System.out.print(t.size() + "\n"); // 4 (#internal nodes in s)
    System.out.print((new Ext<Integer>()).size() + "\n"); // 0
    System.out.print(t.max() + "\n"); // 7
    t.to_stack().print(); // 0 1 5 7
  }
}
