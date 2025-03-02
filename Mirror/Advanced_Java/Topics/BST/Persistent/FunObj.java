public class FunObj {
  public static void main (String[] args) {
    BST<Integer> e = new Ext<Integer>();
    BST<Integer> t = e.add(5).add(1).add(7).add(0).add(5);

    t.print(); // 0 1 5 7
    System.out.println();
  }
}
