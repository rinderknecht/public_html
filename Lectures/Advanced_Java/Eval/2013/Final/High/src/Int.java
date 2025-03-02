public final class Int<Key extends Comparable<Key>>
       extends BST<Key> {

  protected final Key root;
  protected final BST<Key> left, right;

  protected Int (final Key i, final BST<Key> l, final BST<Key> r) {
    assert l != null && r != null;
    root = i; left = l; right = r;
  }

  public Int<Key> add (final Key k) {
    try { return this.add_aux(k); }
    catch (Found x) { return this; }
  }

  protected Int<Key> add_aux (final Key k) throws Found {
    int c = k.compareTo(root);
    if (c == 0) throw new Found();
    return c < 0 ? new Int<Key>(root,left.add_aux(k),right)
                 : new Int<Key>(root,left,right.add_aux(k));
  }

  public boolean isEmpty () { return false; }

  public Pair<Key,BST<Key>> min () { return left.min_aux(this); }

  protected Pair<Key,BST<Key>> min_aux (final Int<Key> p) {
    Pair<Key,BST<Key>> m = left.min_aux(this);
    return new Pair<Key,BST<Key>>(m.fst(),
                               new Int<Key>(p.root,m.snd(),p.right));
  }

  protected BST<Key> min__ (final BST<Key> t) {
    Pair<Key,BST<Key>> m = left.min_aux(this);
    return new Int<Key>(m.fst(),t,m.snd());
  }

  protected BST<Key> rm_aux (final Key k) throws Found {
    int c = k.compareTo(root);
    if (c == 0)
      return left.isEmpty() ? right : right.min__(left);
    return c < 0 ? new Int<Key>(root,left.rm_aux(k),right)
                 : new Int<Key>(root,left,right.rm_aux(k));
  }

  public BST<Key> rm (final Key k) {
    try { return this.rm_aux(k); }
    catch (Found x) { return this; }
  }

  public BST<Key> mirror () {
    return new Int<Key>(root, right.mirror(), left.mirror());
  }

  public void print () {
    System.out.print("(");
    System.out.print("("); left.print(); System.out.print(")");
    System.out.print(root); 
    System.out.print("("); right.print(); System.out.print(")");
    System.out.print(")");
  }
}
