public final class Ext<Key extends Comparable<Key>> 
       extends BST<Key> {

  public final Int<Key> add(final Key k) {
    return this.add_aux(k); }

  protected final Int<Key> add_aux(final Key k) {
    return new Int<Key>(k,this,this); }

  public final Int<Key> add_root(final Key k) {
    return new Int<Key>(k,this,this); }

  public Pair<BST<Key>,BST<Key>> cut(final Key k) {
    return new Pair<BST<Key>,BST<Key>>(this,this);
  }

  public final Int<Key> add_with_cut(final Key k) {
    return new Int<Key>(k,this,this); }

  public final boolean isEmpty() { return true; }

  public final boolean equals(final BST<Key> t) {
    return isEmpty(); }

  protected final boolean eq_aux(final Int<Key> t) {
    return false; }

  public final Pair<Key,BST<Key>> min1() throws EmptyBST {
    throw new EmptyBST(); }

  public final Pair<Key,BST<Key>> min_aux(final Int<Key> p) {
    return new Pair<Key,BST<Key>>(p.root,p.right);
  }

  protected final Stack<Key> post_aux (final Stack<Key> s) {
    return s;
  }

  public void print() {}
}
