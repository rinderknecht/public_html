public class FunObj {
  public static void main (String[] args) {
    BST<Integer> e = new Ext<Integer>();
    BST<Integer> t = 
      e.add(5).add(1).add(2).add(8).add(9).add(6).add(7);

    t.print(); // 1 2 5 6 7 8 9
    System.out.println();
    t.rm(5).print(); // 1 2 6 7 8 9
    System.out.println();
    t.rm(3).print(); // 1 2 5 6 7 8 9
    System.out.println();
  }
}