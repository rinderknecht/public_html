public final class Cycles {

  private Int root;

  public Cycles () { root = null; }

  public void add (final String cycle) throws Prefix {
    if (cycle.length() != 0) {
      if (root == null) root = new Int(root);
      root.insert(root,cycle,0);
    }
  }

  // public Node next (final char c) throws Empty {
  //   if (root == null) throw new Empty ();
  //   return root.next(c);
  // }

}