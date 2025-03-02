public abstract class Stack<Key extends Comparable<Key>> {

  public final NStack<Key> push(final Key k) {
    return new NStack<Key>(k,this); }

  public    abstract Stack<Key>  merge(final Stack<Key> s);
  protected abstract Stack<Key>  merge_aux(final Key k,
                                           final Stack<Key> s,
                                           final NStack<Key> orig);

  public    abstract void        print();
}
