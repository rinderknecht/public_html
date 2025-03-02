public abstract class Stack<Key> {

  public final NStack<Key> push (final Key k) {
    return new NStack<Key>(k,this); }

  public abstract boolean isEmpty ();

  public final Stack<Key> rev () { 
    return rev_cat(new EStack<Key>()); }

  public abstract Stack<Key> rev_cat (final Stack<Key> s);

  public abstract Queue<Key> mkq (final Stack<Key> r);

  public abstract void print ();

  public abstract void rev_print ();
}
