public abstract class BST<Key extends Comparable<Key>> {
  public    abstract Int<Key> add(final Key k);
  protected abstract Int<Key> add_aux(final Key k) throws Found;
  public    abstract boolean isEmpty();
  protected abstract BST<Key> min__ (final BST<Key> t);
  public    abstract BST<Key> rm(final Key k);
  protected abstract Pair<Key,BST<Key>> min_aux(final Int<Key> p);
  protected abstract BST<Key> rm_aux (final Key k) throws Found;
  public    abstract BST<Key> mirror ();
  public    abstract void print();
}
