public abstract class Stack<Key extends Comparable<Key>> {

  public final NStack<Key> push(final Key k) {
    return new NStack<Key>(k,this); }

  public abstract boolean isEmpty ();

  public final Stack<Key> rev() { 
    return rev_cat(new EStack<Key>()); }

  public    abstract Stack<Key>  cat(final Stack<Key> s);
  public    abstract Stack<Key>  rev_cat(final Stack<Key> s);
  public    abstract Stack<Key>  isort();
  protected abstract NStack<Key> insert(final Key k);
  public    abstract Stack<Key>  merge(final Stack<Key> s);
  protected abstract Stack<Key>  merge_aux(final Key k,
                                           final Stack<Key> s,
                                           final NStack<Key> orig);

  public    abstract void        print();
}
