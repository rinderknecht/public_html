public final class EStack<Key> extends Stack<Key> {

  public final boolean isEmpty () { return true; }

  public final Stack<Key> rev_cat(final Stack<Key> s) { 
    return s; }

  public Queue<Key> mkq (final Stack<Key> r) {
    if (r.isEmpty()) return new EQueue<Key>();
    return new NQueue<Key>((NStack<Key>) r.rev(),this);
  }

  public void print () { }

  public void rev_print () { }

}
