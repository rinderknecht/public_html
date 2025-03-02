public abstract class BST<Key extends Comparable<Key>> {
  public    abstract Int<Key> add (final Key k);
  protected abstract Int<Key> add_aux (final Key k) throws Found;
  public    abstract Int<Key> add_root (final Key k) throws Found;
  public    abstract Pair<BST<Key>,BST<Key>> cut (final Key k) 
    throws Found;
  public    abstract Int<Key> add_with_cut (final Key k)
    throws Found;
  public    abstract boolean isEmpty ();
  public    abstract boolean equals (final BST<Key> t);
  protected abstract boolean eq_aux (final Int<Key> t);
  public    abstract Pair<Key,BST<Key>> min1 () throws EmptyBST;
  public    abstract Pair<Key,BST<Key>> min_aux (final Int<Key> p);
  public    Stack<Key> postorder() {
    return post_aux(new EStack<Key>()); }
  protected abstract Stack<Key> post_aux (final Stack<Key> s);
  public    abstract void print ();
}
