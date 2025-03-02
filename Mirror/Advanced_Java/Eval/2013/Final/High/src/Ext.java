public final class Ext<Key extends Comparable<Key>> 
       extends BST<Key> {

  public final Int<Key> add (final Key k) {
    return this.add_aux(k); }

  protected final Int<Key> add_aux (final Key k) {
    return new Int<Key>(k,this,this); }

  public boolean isEmpty () { return true; }

  protected final Pair<Key,BST<Key>> min_aux (final Int<Key> p) {
    return new Pair<Key,BST<Key>>(p.root,p.right); }

  protected BST<Key> min__ (final BST<Key> t) { return t; }

  public final BST<Key> rm (final Key k) {
    return new Int<Key>(k,this,this); }

  protected final BST<Key> rm_aux (final Key k) throws Found {
    throw new Found(); }

  public final BST<Key> mirror () { return this; }

  public void print() {}
}
