public final class Int<Key extends Comparable<Key>>
       extends BST<Key> {

  protected final Key root;
  protected final BST<Key> left, right;

  protected Int(final Key i, final BST<Key> l, final BST<Key> r) {
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

  public final Int<Key> add_root (final Key k) throws Found {
    int c = k.compareTo(root);
    if (c == 0) throw new Found();
    if (c < 0) {
      Int<Key> t = left.add_root(k);
      return new Int<Key>(t.root,
                          t.left,
                          new Int<Key>(root,t.right,right));
    }
    Int<Key> t = right.add_root(k);
    return new Int<Key>(t.root,
                        new Int<Key>(root,left,t.left),
                        t.right); 
  }

  public Pair<BST<Key>,BST<Key>> cut (final Key k) throws Found {
    int c = k.compareTo(root);
    if (c == 0) throw new Found();
    if (c < 0) {
      Pair<BST<Key>,BST<Key>> p = left.cut(k);
      return new Pair<BST<Key>,BST<Key>>
        (p.fst(), new Int<Key>(root,p.snd(),right));
    }
    Pair<BST<Key>,BST<Key>> p = right.cut(k);
      return new Pair<BST<Key>,BST<Key>>
        (new Int<Key>(root,left,p.fst()), p.snd());
  }

  public final Int<Key> add_with_cut (final Key k) throws Found {
    Pair<BST<Key>,BST<Key>> p = cut(k);
    return new Int<Key>(k,p.fst(),p.snd());
  }

  public final boolean equals (final BST<Key> t) {
    return t.eq_aux(this); }

  protected boolean eq_aux (final Int<Key> t) {
    return this.root.compareTo(t.root) == 0
      && this.left.equals(t.left)
      && this.right.equals(t.right);
  }

  public Pair<Key,BST<Key>> min1 () throws EmptyBST {
    try {
      Pair<Key,BST<Key>> m = left.min1();
      return new Pair<Key,BST<Key>>(m.fst(),
                                   new Int<Key>(root,m.snd(),right));
    }
    catch (EmptyBST x) { return new Pair<Key,BST<Key>>(root,right); }
  }

  public Pair<Key,BST<Key>> min_aux (final Int<Key> p) {
    Pair<Key,BST<Key>> m = left.min_aux(this);
    return new Pair<Key,BST<Key>>(m.fst(),
                               new Int<Key>(p.root,m.snd(),p.right));
  }

  public boolean isEmpty () { return false; }

  public Pair<Key,BST<Key>> min () { return left.min_aux(this); }

  protected Stack<Key> post_aux (final Stack<Key> s) {
    return left.post_aux(right.post_aux(s.push(root)));
  }

  public void print () { 
    left.print(); System.out.print(root + " "); right.print(); }
}
